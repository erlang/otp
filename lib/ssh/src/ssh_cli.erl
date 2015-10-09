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
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_cli).

-behaviour(ssh_daemon_channel).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% ssh_channel callbacks
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2]).

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
handle_ssh_msg({ssh_cm, _ConnectionHandler,
		{data, _ChannelId, _Type, Data}}, 
	       #state{group = Group} = State) ->
    List = binary_to_list(Data),
    to_group(List, Group),
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionHandler,
		{pty, ChannelId, WantReply, 
		 {TermName, Width, Height, PixWidth, PixHeight, Modes}}}, 
	       State0) ->
    State = State0#state{pty = 
			 #ssh_pty{term = TermName,
				  width =  not_zero(Width, 80),
				  height = not_zero(Height, 24),
				  pixel_width = PixWidth,
				  pixel_height = PixHeight,
                  modes = Modes},
             buf = empty_buf()},
    set_echo(State),
    ssh_connection:reply_request(ConnectionHandler, WantReply,
				 success, ChannelId),
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionHandler,
	    {env, ChannelId, WantReply, _Var, _Value}}, State) ->
    ssh_connection:reply_request(ConnectionHandler,
				 WantReply, failure, ChannelId),
    {ok, State};

handle_ssh_msg({ssh_cm, ConnectionHandler,
	    {window_change, ChannelId, Width, Height, PixWidth, PixHeight}},
	   #state{buf = Buf, pty = Pty0} = State) ->
    Pty = Pty0#ssh_pty{width = Width, height = Height,
		       pixel_width = PixWidth,
		       pixel_height = PixHeight},
    {Chars, NewBuf} = io_request({window_change, Pty0}, Buf, Pty, undefined),
    write_chars(ConnectionHandler, ChannelId, Chars),
    {ok, State#state{pty = Pty, buf = NewBuf}};

handle_ssh_msg({ssh_cm, ConnectionHandler,
	    {shell, ChannelId, WantReply}}, State) ->
    NewState = start_shell(ConnectionHandler, State),
    ssh_connection:reply_request(ConnectionHandler, WantReply,
				 success, ChannelId),
    {ok, NewState#state{channel = ChannelId,
			cm = ConnectionHandler}};

handle_ssh_msg({ssh_cm, ConnectionHandler,
		{exec, ChannelId, WantReply, Cmd}}, #state{exec=undefined} = State) ->
    {Reply, Status} = exec(Cmd),
    write_chars(ConnectionHandler,
		ChannelId, io_lib:format("~p\n", [Reply])),
    ssh_connection:reply_request(ConnectionHandler, WantReply,
				 success, ChannelId),
    ssh_connection:exit_status(ConnectionHandler, ChannelId, Status),
    ssh_connection:send_eof(ConnectionHandler, ChannelId),
    {stop, ChannelId, State#state{channel = ChannelId, cm = ConnectionHandler}};
handle_ssh_msg({ssh_cm, ConnectionHandler,
		{exec, ChannelId, WantReply, Cmd}}, State) ->
    NewState = start_shell(ConnectionHandler, Cmd, State),
    ssh_connection:reply_request(ConnectionHandler, WantReply,
				 success, ChannelId),
    {ok, NewState#state{channel = ChannelId,
			cm = ConnectionHandler}};

handle_ssh_msg({ssh_cm, _ConnectionHandler, {eof, _ChannelId}}, State) ->
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
handle_msg({ssh_channel_up, ChannelId, ConnectionHandler},
	   #state{channel = ChannelId,
		  cm = ConnectionHandler} = State) ->
    {ok,  State};

handle_msg({Group, set_unicode_state, _Arg}, State) ->
    Group ! {self(), set_unicode_state, false},
    {ok, State};

handle_msg({Group, get_unicode_state}, State) ->
    Group ! {self(), get_unicode_state, false},
    {ok, State};

handle_msg({Group, tty_geometry}, #state{group = Group,
					 pty = Pty
					} = State) ->
    case Pty of
	#ssh_pty{width=Width,height=Height} ->
	    Group ! {self(),tty_geometry,{Width,Height}};
	_ ->
	    %% This is a dirty fix of the problem with the otp ssh:shell
	    %% client. That client will not allocate a tty, but someone
	    %% asks for the tty_geometry just before every erlang prompt.
	    %% If that question is not answered, there is a 2 sec timeout
	    %% Until the prompt is seen by the user at the client side ...
	    Group ! {self(),tty_geometry,{0,0}}
    end,
    {ok,State};
    
handle_msg({Group, Req}, #state{group = Group, buf = Buf, pty = Pty,
				 cm = ConnectionHandler,
				 channel = ChannelId} = State) ->
    {Chars, NewBuf} = io_request(Req, Buf, Pty, Group),
    write_chars(ConnectionHandler, ChannelId, Chars),
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

to_group([], _Group) ->
    ok;
to_group([$\^C | Tail], Group) ->
    exit(Group, interrupt),
    to_group(Tail, Group);
to_group(Data, Group) ->
    Func = fun(C) -> C /= $\^C end,
    Tail = case lists:splitwith(Func, Data) of
        {[], Right} ->
            Right;
        {Left, Right} ->
            Group ! {self(), {data, Left}},
            Right
    end,
    to_group(Tail, Group).

exec(Cmd) ->
    case eval(parse(scan(Cmd))) of
	{error, _} ->
	    {Cmd, 0}; %% This should be an external call
	Term ->
	    Term
    end.

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
io_request({window_change, OldTty}, Buf, Tty, _Group) ->
    window_change(Tty, OldTty, Buf);
io_request({put_chars, Cs}, Buf, Tty, _Group) ->
    put_chars(bin_to_list(Cs), Buf, Tty);
io_request({put_chars, unicode, Cs}, Buf, Tty, _Group) ->
    put_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({insert_chars, Cs}, Buf, Tty, _Group) ->
    insert_chars(bin_to_list(Cs), Buf, Tty);
io_request({insert_chars, unicode, Cs}, Buf, Tty, _Group) ->
    insert_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({move_rel, N}, Buf, Tty, _Group) ->
    move_rel(N, Buf, Tty);
io_request({delete_chars,N}, Buf, Tty, _Group) ->
    delete_chars(N, Buf, Tty);
io_request(beep, Buf, _Tty, _Group) ->
    {[7], Buf};

%% New in R12
io_request({get_geometry,columns},Buf,Tty, _Group) ->
    {ok, Tty#ssh_pty.width, Buf};
io_request({get_geometry,rows},Buf,Tty, _Group) ->
    {ok, Tty#ssh_pty.height, Buf};
io_request({requests,Rs}, Buf, Tty, Group) ->
    io_requests(Rs, Buf, Tty, [], Group);
io_request(tty_geometry, Buf, Tty, Group) ->
    io_requests([{move_rel, 0}, {put_chars, unicode, [10]}],
                Buf, Tty, [], Group);
     %{[], Buf};

%% New in 18
io_request({put_chars_sync, Class, Cs, Reply}, Buf, Tty, Group) ->
    %% We handle these asynchronous for now, if we need output guarantees
    %% we have to handle these synchronously
    Group ! {reply, Reply},
    io_request({put_chars, Class, Cs}, Buf, Tty, Group);

io_request(_R, Buf, _Tty, _Group) ->
    {[], Buf}.

io_requests([R|Rs], Buf, Tty, Acc, Group) ->
    {Chars, NewBuf} = io_request(R, Buf, Tty, Group),
    io_requests(Rs, NewBuf, Tty, [Acc|Chars], Group);
io_requests([], Buf, _Tty, Acc, _Group) ->
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
    NewCol = case Col + N of V when V >= 0 -> V; _ -> 0 end,
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
write_chars(ConnectionHandler, ChannelId, Chars) ->
    case erlang:iolist_size(Chars) of
	0 ->
	    ok;
       _ ->
	    ssh_connection:send(ConnectionHandler, ChannelId,
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

start_shell(ConnectionHandler, State) ->
    Shell = State#state.shell,
    ConnectionInfo = ssh_connection_handler:connection_info(ConnectionHandler,
						  [peer, user]),
    ShellFun = case is_function(Shell) of
		   true ->
		       User = 
			   proplists:get_value(user, ConnectionInfo),
		       case erlang:fun_info(Shell, arity) of
			   {arity, 1} ->
			       fun() -> Shell(User) end;
			   {arity, 2} ->
			       {_, PeerAddr} =
				   proplists:get_value(peer, ConnectionInfo),
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

start_shell(_ConnectionHandler, Cmd, #state{exec={M, F, A}} = State) ->
    Group = group:start(self(), {M, F, A++[Cmd]}, [{echo, false}]),
    State#state{group = Group, buf = empty_buf()};
start_shell(ConnectionHandler, Cmd, #state{exec=Shell} = State) when is_function(Shell) ->

    ConnectionInfo = ssh_connection_handler:connection_info(ConnectionHandler,
						 [peer, user]),
    User = 
	proplists:get_value(user, ConnectionInfo),
    ShellFun = 
	case erlang:fun_info(Shell, arity) of
	    {arity, 1} ->
		fun() -> Shell(Cmd) end;
	    {arity, 2} ->
		fun() -> Shell(Cmd, User) end;
	    {arity, 3} ->
		{_, PeerAddr} =
		    proplists:get_value(peer, ConnectionInfo),
		fun() -> Shell(Cmd, User, PeerAddr) end;
	    _ ->
		Shell
	end,
    Echo = get_echo(State#state.pty),
    Group = group:start(self(), ShellFun, [{echo,Echo}]),
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

