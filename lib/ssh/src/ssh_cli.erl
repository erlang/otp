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
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_cli).

-behaviour(ssh_channel).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% ssh_channel callbacks
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2]).

%% backwards compatibility
-export([listen/1, listen/2, listen/3, listen/4, stop/1]).

%% state
-record(state, {
	  cm,
	  channel,
	  pty,
	  group,
	  buf,
	  shell,
	  exec
	 }).

%%====================================================================
%% ssh_channel callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} 
%%                        
%% Description: Initiates the CLI
%%--------------------------------------------------------------------
init([Shell, Exec]) ->
    {ok, #state{shell = Shell, exec = Exec}};
init([Shell]) ->
    {ok, #state{shell = Shell}}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages received on the ssh-connection.
%%--------------------------------------------------------------------
handle_ssh_msg({ssh_cm, _ConnectionManager, 
		{data, _ChannelId, _Type, Data}}, 
	       #state{group = Group} = State) ->
    Group ! {self(), {data, binary_to_list(Data)}},
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionManager, 
		{pty, ChannelId, WantReply, 
		 {TermName, Width, Height, PixWidth, PixHeight, Modes}}}, 
	       State0) ->
    State = State0#state{pty = 
			 #ssh_pty{term = TermName,
				  width =  not_zero(Width, 80),
				  height = not_zero(Height, 24),
				  pixel_width = PixWidth,
				  pixel_height = PixHeight,
				  modes = Modes}},
    set_echo(State),
    ssh_connection:reply_request(ConnectionManager, WantReply, 
				 success, ChannelId),
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionManager, 
	    {env, ChannelId, WantReply, _Var, _Value}}, State) ->
    ssh_connection:reply_request(ConnectionManager, 
				 WantReply, failure, ChannelId),
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionManager,
	    {window_change, ChannelId, Width, Height, PixWidth, PixHeight}},
	   #state{buf = Buf, pty = Pty0} = State) ->
    Pty = Pty0#ssh_pty{width = Width, height = Height,
		       pixel_width = PixWidth,
		       pixel_height = PixHeight},
    {Chars, NewBuf} = io_request({window_change, Pty0}, Buf, Pty),
    write_chars(ConnectionManager, ChannelId, Chars),
    {ok, State#state{pty = Pty, buf = NewBuf}};

handle_ssh_msg({ssh_cm, ConnectionManager, 
	    {shell, ChannelId, WantReply}}, State) ->
    NewState = start_shell(ConnectionManager, State),
    ssh_connection:reply_request(ConnectionManager, WantReply, 
				 success, ChannelId),
    {ok, NewState#state{channel = ChannelId,
			cm = ConnectionManager}};

handle_ssh_msg({ssh_cm, ConnectionManager, 
		{exec, ChannelId, WantReply, Cmd}}, #state{exec=undefined} = State) ->
    {Reply, Status} = exec(Cmd),
    write_chars(ConnectionManager, 
		ChannelId, io_lib:format("~p\n", [Reply])),
    ssh_connection:reply_request(ConnectionManager, WantReply, 
				 success, ChannelId),
    ssh_connection:exit_status(ConnectionManager, ChannelId, Status),
    ssh_connection:send_eof(ConnectionManager, ChannelId),
    {stop, ChannelId, State#state{channel = ChannelId, cm = ConnectionManager}};
handle_ssh_msg({ssh_cm, ConnectionManager,
		{exec, ChannelId, WantReply, Cmd}}, State) ->
    NewState = start_shell(ConnectionManager, Cmd, State),
    ssh_connection:reply_request(ConnectionManager, WantReply,
				 success, ChannelId),
    {ok, NewState#state{channel = ChannelId,
			cm = ConnectionManager}};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    Report = io_lib:format("Connection closed by peer ~n Error ~p~n",
			   [Error]),
    error_logger:error_report(Report),
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    
    Report = io_lib:format("Connection closed by peer ~n Status ~p~n",
			   [Status]),
    error_logger:error_report(Report),
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles other channel messages.
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChannelId, ConnectionManager},
	   #state{channel = ChannelId,
		  cm = ConnectionManager} = State) ->
    {ok,  State};

handle_msg({Group, Req}, #state{group = Group, buf = Buf, pty = Pty,
				 cm = ConnectionManager,
				 channel = ChannelId} = State) ->
    {Chars, NewBuf} = io_request(Req, Buf, Pty),
    write_chars(ConnectionManager, ChannelId, Chars),
    {ok, State#state{buf = NewBuf}};

handle_msg({'EXIT', Group, _Reason}, #state{group = Group,
					     channel = ChannelId} = State) ->
    {stop, ChannelId, State};

handle_msg(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: Called when the channel process is trminated
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

exec(Cmd) ->
    eval(parse(scan(Cmd))).

scan(Cmd) ->
    erl_scan:string(Cmd). 

parse({ok, Tokens, _}) ->
    erl_parse:parse_exprs(Tokens);
parse(Error) ->
    Error.

eval({ok, Expr_list}) ->
    case (catch erl_eval:exprs(Expr_list,
 			       erl_eval:new_bindings())) of
 	{value, Value, _NewBindings} ->
 	    {Value, 0};
 	{'EXIT', {Error, _}} -> 
 	    {Error, -1};
 	Error -> 
 	    {Error, -1}
    end;
eval(Error) ->
    {Error, -1}.

%%% io_request, handle io requests from the user process,
%%% Note, this is not the real I/O-protocol, but the mockup version
%%% used between edlin and a user_driver. The protocol tags are
%%% similar, but the message set is different. 
%%% The protocol only exists internally between edlin and a character
%%% displaying device...
%%% We are *not* really unicode aware yet, we just filter away characters 
%%% beyond the latin1 range. We however handle the unicode binaries...
io_request({window_change, OldTty}, Buf, Tty) ->
    window_change(Tty, OldTty, Buf);
io_request({put_chars, Cs}, Buf, Tty) ->
    put_chars(bin_to_list(Cs), Buf, Tty);
io_request({put_chars, unicode, Cs}, Buf, Tty) ->
    put_chars([Ch || Ch <- unicode:characters_to_list(Cs,unicode), Ch =< 255], Buf, Tty);
io_request({insert_chars, Cs}, Buf, Tty) ->
    insert_chars(bin_to_list(Cs), Buf, Tty);
io_request({insert_chars, unicode, Cs}, Buf, Tty) ->
    insert_chars([Ch || Ch <- unicode:characters_to_list(Cs,unicode), Ch =< 255], Buf, Tty);
io_request({move_rel, N}, Buf, Tty) ->
    move_rel(N, Buf, Tty);
io_request({delete_chars,N}, Buf, Tty) ->
    delete_chars(N, Buf, Tty);
io_request(beep, Buf, _Tty) ->
    {[7], Buf};

%% New in R12
io_request({get_geometry,columns},Buf,Tty) ->
    {ok, Tty#ssh_pty.width, Buf};
io_request({get_geometry,rows},Buf,Tty) ->
    {ok, Tty#ssh_pty.height, Buf};
io_request({requests,Rs}, Buf, Tty) ->
    io_requests(Rs, Buf, Tty, []);
io_request(tty_geometry, Buf, Tty) ->
    io_requests([{move_rel, 0}, {put_chars, unicode, [10]}], Buf, Tty, []);
     %{[], Buf};
io_request(_R, Buf, _Tty) ->
    {[], Buf}.

io_requests([R|Rs], Buf, Tty, Acc) ->
    {Chars, NewBuf} = io_request(R, Buf, Tty),
    io_requests(Rs, NewBuf, Tty, [Acc|Chars]);
io_requests([], Buf, _Tty, Acc) ->
    {Acc, Buf}.

%%% return commands for cursor navigation, assume everything is ansi
%%% (vt100), add clauses for other terminal types if needed
ansi_tty(N, L) ->
    ["\e[", integer_to_list(N), L].

get_tty_command(up, N, _TerminalType) ->
    ansi_tty(N, $A);
get_tty_command(down, N, _TerminalType) ->
    ansi_tty(N, $B);
get_tty_command(right, N, _TerminalType) ->
    ansi_tty(N, $C);
get_tty_command(left, N, _TerminalType) ->
    ansi_tty(N, $D).


-define(PAD, 10).
-define(TABWIDTH, 8).

%% convert input characters to buffer and to writeout
%% Note that the buf is reversed but the buftail is not
%% (this is handy; the head is always next to the cursor)
conv_buf([], AccBuf, AccBufTail, AccWrite, Col) ->
    {AccBuf, AccBufTail, lists:reverse(AccWrite), Col};
conv_buf([13, 10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl2(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([13 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [13 | AccWrite], 0);
conv_buf([10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([C | Rest], AccBuf, AccBufTail, AccWrite, Col) ->
    conv_buf(Rest, [C | AccBuf], tl1(AccBufTail), [C | AccWrite], Col + 1).


%%% put characters at current position (possibly overwriting
%%% characters after current position in buffer)
put_chars(Chars, {Buf, BufTail, Col}, _Tty) ->
    {NewBuf, NewBufTail, WriteBuf, NewCol} =
	conv_buf(Chars, Buf, BufTail, [], Col),
    {WriteBuf, {NewBuf, NewBufTail, NewCol}}.

%%% insert character at current position
insert_chars([], {Buf, BufTail, Col}, _Tty) ->
    {[], {Buf, BufTail, Col}};
insert_chars(Chars, {Buf, BufTail, Col}, Tty) ->
    {NewBuf, _NewBufTail, WriteBuf, NewCol} =
	conv_buf(Chars, Buf, [], [], Col),
    M = move_cursor(NewCol + length(BufTail), NewCol, Tty),
    {[WriteBuf, BufTail | M], {NewBuf, BufTail, NewCol}}.

%%% delete characters at current position, (backwards if negative argument)
delete_chars(0, {Buf, BufTail, Col}, _Tty) ->
    {[], {Buf, BufTail, Col}};
delete_chars(N, {Buf, BufTail, Col}, Tty) when N > 0 ->
    NewBufTail = nthtail(N, BufTail),
    M = move_cursor(Col + length(NewBufTail) + N, Col, Tty),
    {[NewBufTail, lists:duplicate(N, $ ) | M],
     {Buf, NewBufTail, Col}};
delete_chars(N, {Buf, BufTail, Col}, Tty) -> % N < 0
    NewBuf = nthtail(-N, Buf),
    NewCol = Col + N,
    M1 = move_cursor(Col, NewCol, Tty),
    M2 = move_cursor(NewCol + length(BufTail) - N, NewCol, Tty),
    {[M1, BufTail, lists:duplicate(-N, $ ) | M2],
     {NewBuf, BufTail, NewCol}}.

%%% Window change, redraw the current line (and clear out after it
%%% if current window is wider than previous)
window_change(Tty, OldTty, Buf)
  when OldTty#ssh_pty.width == Tty#ssh_pty.width ->
    {[], Buf};
window_change(Tty, OldTty, {Buf, BufTail, Col}) ->
    M1 = move_cursor(Col, 0, OldTty),
    N = erlang:max(Tty#ssh_pty.width - OldTty#ssh_pty.width, 0) * 2,
    S = lists:reverse(Buf, [BufTail | lists:duplicate(N, $ )]),
    M2 = move_cursor(length(Buf) + length(BufTail) + N, Col, Tty),
    {[M1, S | M2], {Buf, BufTail, Col}}.
    
%% move around in buffer, respecting pad characters
step_over(0, Buf, [?PAD | BufTail], Col) ->
    {[?PAD | Buf], BufTail, Col+1};
step_over(0, Buf, BufTail, Col) ->
    {Buf, BufTail, Col};
step_over(N, [C | Buf], BufTail, Col) when N < 0 ->
    N1 = ifelse(C == ?PAD, N, N+1),
    step_over(N1, Buf, [C | BufTail], Col-1);
step_over(N, Buf, [C | BufTail], Col) when N > 0 ->
    N1 = ifelse(C == ?PAD, N, N-1),
    step_over(N1, [C | Buf], BufTail, Col+1).

%%% an empty line buffer
empty_buf() -> {[], [], 0}.

%%% col and row from position with given width
col(N, W) -> N rem W.
row(N, W) -> N div W.

%%% move relative N characters
move_rel(N, {Buf, BufTail, Col}, Tty) ->
    {NewBuf, NewBufTail, NewCol} = step_over(N, Buf, BufTail, Col),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewBuf, NewBufTail, NewCol}}.

%%% give move command for tty
move_cursor(A, A, _Tty) ->
    [];
move_cursor(From, To, #ssh_pty{width=Width, term=Type}) ->
    Tcol = case col(To, Width) - col(From, Width) of
	       0 -> "";
	       I when I < 0 -> get_tty_command(left, -I, Type);
	       I -> get_tty_command(right, I, Type)
	end,
    Trow = case row(To, Width) - row(From, Width) of
	       0 -> "";
	       J when J < 0 -> get_tty_command(up, -J, Type);
	       J -> get_tty_command(down, J, Type)
	   end,
    [Tcol | Trow].

%% %%% write out characters
%% %%% make sure that there is data to send
%% %%% before calling ssh_connection:send
write_chars(ConnectionManager, ChannelId, Chars) ->
    case erlang:iolist_size(Chars) of
	0 ->
	    ok;
       _ ->
	    ssh_connection:send(ConnectionManager, ChannelId, 
				?SSH_EXTENDED_DATA_DEFAULT, Chars)
    end.

%%% tail, works with empty lists
tl1([_|A]) -> A;
tl1(_) -> [].

%%% second tail
tl2([_,_|A]) -> A;
tl2(_) -> [].

%%% nthtail as in lists, but no badarg if n > the length of list
nthtail(0, A) -> A;
nthtail(N, [_ | A]) when N > 0 -> nthtail(N-1, A);
nthtail(_, _) -> [].

ifelse(Cond, A, B) ->
    case Cond of
	true -> A;
	_ -> B
    end.	    

bin_to_list(B) when is_binary(B) ->
    binary_to_list(B);
bin_to_list(L) when is_list(L) ->
    lists:flatten([bin_to_list(A) || A <- L]);
bin_to_list(I) when is_integer(I) ->
    I.

start_shell(ConnectionManager, State) ->
    Shell = State#state.shell,
    ShellFun = case is_function(Shell) of
		   true ->
		       case erlang:fun_info(Shell, arity) of
			   {arity, 1} ->
			       {ok, User} = 
				   ssh_userreg:lookup_user(ConnectionManager),
			       fun() -> Shell(User) end;
			   {arity, 2} ->
			       {ok, User} = 
				   ssh_userreg:lookup_user(ConnectionManager),
			       {ok, PeerAddr} = 
				   ssh_connection_manager:peer_addr(ConnectionManager),
			       fun() -> Shell(User, PeerAddr) end;
			   _ ->
			       Shell
		       end;
		   _ ->
		       Shell
	       end,
    Echo = get_echo(State#state.pty),
    Group = group:start(self(), ShellFun, [{echo, Echo}]),
    State#state{group = Group, buf = empty_buf()}.

start_shell(_ConnectionManager, Cmd, #state{exec={M, F, A}} = State) ->
    Group = group:start(self(), {M, F, A++[Cmd]}, [{echo,false}]),
    State#state{group = Group, buf = empty_buf()}.


% Pty can be undefined if the client never sets any pty options before
% starting the shell.
get_echo(undefined) ->
    true;
get_echo(#ssh_pty{modes = Modes}) ->
    case proplists:get_value(echo, Modes, 1) of 
	0 ->
	    false;
	_ ->
	    true
    end.

% Group is undefined if the pty options are sent between open and
% shell messages.
set_echo(#state{group = undefined}) ->
    ok;
set_echo(#state{group = Group, pty = Pty}) ->
    Echo = get_echo(Pty),
    Group ! {self(), echo, Echo}.

not_zero(0, B) -> 
    B;
not_zero(A, _) -> 
    A.

%%% Backwards compatibility
	    
%%--------------------------------------------------------------------
%% Function: listen(...) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts a listening server
%% Note that the pid returned is NOT the pid of this gen_server;
%% this server is started when an SSH connection is made on the
%% listening port
%%--------------------------------------------------------------------
listen(Shell) ->
    listen(Shell, 22).

listen(Shell, Port) ->
    listen(Shell, Port, []).

listen(Shell, Port, Opts) ->
    listen(Shell, any, Port, Opts).

listen(Shell, HostAddr, Port, Opts) ->
    ssh:daemon(HostAddr, Port, [{shell, Shell} | Opts]).
    

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop(Pid) ->
    ssh:stop_listener(Pid).
