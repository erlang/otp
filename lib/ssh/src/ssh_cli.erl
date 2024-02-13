%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2024. All Rights Reserved.
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
-moduledoc false.

-behaviour(ssh_server_channel).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% ssh_server_channel callbacks
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2]).

%% state
-record(state, {
	  cm,
	  channel,
	  pty,
          encoding,
          deduced_encoding, % OpenSSH sometimes lies about its encodeing. This variable
                            % is for the process of guessing the peer encoding, taylord
                            % after the behaviour of openssh.  If it says latin1 it is so.
                            % It there arrives characters encoded in latin1 it is so. Otherwise
                            % assume utf8 until otherwise is proved.
	  group,
	  buf,
	  shell,
	  exec
	 }).

-define(EXEC_ERROR_STATUS, 255).

%%====================================================================
%% ssh_server_channel callbacks
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
	       #state{group = Group} = State0) ->
    {Enc, State} = guess_encoding(Data, State0),
    List = unicode:characters_to_list(Data, Enc),
    to_group(List, Group, get_dumb(State#state.pty)),
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
	    {env, ChannelId, WantReply, Var, Value}}, State = #state{encoding=Enc0}) ->
    %% It is not as simple as it sounds to set environment variables
    %% in the server.
    %% The (OS) env vars should be per per Channel; otherwise anyone
    %% could affect anyone other's variables.
    %% Therefore this clause always return a failure.
    ssh_connection:reply_request(ConnectionHandler,
				 WantReply, failure, ChannelId),

    %% https://pubs.opengroup.org/onlinepubs/7908799/xbd/envvar.html
    %% LANG
    %%     This variable determines the locale category for native language,
    %%     local customs and coded character set in the absence of the LC_ALL
    %%     and other LC_* (LC_COLLATE, LC_CTYPE, LC_MESSAGES, LC_MONETARY,
    %%     LC_NUMERIC, LC_TIME) environment variables. This can be used by
    %%     applications to determine the language to use for error messages
    %%     and instructions, collating sequences, date formats, and so forth. 
    %% LC_ALL
    %%     This variable determines the values for all locale categories. The
    %%     value of the LC_ALL environment variable has precedence over any of
    %%     the other environment variables starting with LC_ (LC_COLLATE,
    %%     LC_CTYPE, LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME) and the
    %%     LANG environment variable.
    %% ...
    %%
    %%  The values of locale categories are determined by a precedence order;
    %%  the first condition met below determines the value:
    %%
    %%   1. If the LC_ALL environment variable is defined and is not null,
    %%      the value of LC_ALL is used.
    %%
    %%   2. If the LC_* environment variable ( LC_COLLATE, LC_CTYPE, LC_MESSAGES,
    %%      LC_MONETARY, LC_NUMERIC, LC_TIME) is defined and is not null, the
    %%      value of the environment variable is used to initialise the category
    %%      that corresponds to the environment variable.
    %%
    %%   3. If the LANG environment variable is defined and is not null, the value
    %%      of the LANG environment variable is used.
    %%
    %%   4. If the LANG environment variable is not set or is set to the empty string,
    %%      the implementation-dependent default locale is used. 

    Enc =
        %% Rule 1 and 3 above says that LC_ALL has precedence over LANG. Since they
        %% arrives in different messages and in an undefined order, it is resolved
        %% like this:
        case Var of
            <<"LANG">> when Enc0==undefined ->
                %% No previous LC_ALL
                case claim_encoding(Value) of
                    {ok,Enc1} -> Enc1;
                    _ -> Enc0
                end;
            <<"LC_ALL">> ->
                %% Maybe or maybe not a LANG has been handled, LC_ALL doesn't care
                case claim_encoding(Value) of
                    {ok,Enc1} -> Enc1;
                    _ -> Enc0
                end;
            _ ->
                Enc0
        end,
    {ok, State#state{encoding=Enc}};

handle_ssh_msg({ssh_cm, ConnectionHandler,
	    {window_change, ChannelId, Width, Height, PixWidth, PixHeight}},
	   #state{buf = Buf, pty = Pty0} = State) ->
    Pty = Pty0#ssh_pty{width = Width, height = Height,
		       pixel_width = PixWidth,
		       pixel_height = PixHeight},
    {Chars, NewBuf} = io_request({window_change, Pty0}, Buf, Pty, undefined),
    write_chars(ConnectionHandler, ChannelId, Chars),
    {ok, State#state{pty = Pty, buf = NewBuf}};

handle_ssh_msg({ssh_cm, ConnectionHandler,  {shell, ChannelId, WantReply}}, #state{shell=disabled} = State) ->
    write_chars(ConnectionHandler, ChannelId, 1, "Prohibited."),
    ssh_connection:reply_request(ConnectionHandler, WantReply, success, ChannelId),
    ssh_connection:exit_status(ConnectionHandler, ChannelId, ?EXEC_ERROR_STATUS),
    ssh_connection:send_eof(ConnectionHandler, ChannelId),
    {stop, ChannelId, State#state{channel = ChannelId, cm = ConnectionHandler}};
handle_ssh_msg({ssh_cm, ConnectionHandler,  {shell, ChannelId, WantReply}}, State0) ->
    State = case State0#state.encoding of
                undefined -> State0#state{encoding = utf8};
                _-> State0
            end,
    NewState = start_shell(ConnectionHandler, State),
    ssh_connection:reply_request(ConnectionHandler, WantReply, success, ChannelId),
    {ok, NewState#state{channel = ChannelId,
			cm = ConnectionHandler}};

handle_ssh_msg({ssh_cm, ConnectionHandler,  {exec, ChannelId, WantReply, Cmd0}}, S0) ->
    {Enc,S1} = guess_encoding(Cmd0, S0),
    Cmd = unicode:characters_to_list(Cmd0, Enc),
    case
        case S1#state.exec of
            disabled ->
                {"Prohibited.", ?EXEC_ERROR_STATUS, 1};

            {direct,F} ->
                %% Exec called and a Fun or MFA is defined to use.  The F returns the
                %% value to return.
                %% The standard I/O is directed from/to the channel ChannelId.
                exec_direct(ConnectionHandler, ChannelId, Cmd, F, WantReply, S1);

            undefined when S0#state.shell == ?DEFAULT_SHELL ; 
                           S0#state.shell == disabled ->
                %% Exec called and the shell is the default shell (= Erlang shell).
                %% To be exact, eval the term as an Erlang term (but not using the
                %% ?DEFAULT_SHELL directly). This disables banner, prompts and such.
                %% The standard I/O is directed from/to the channel ChannelId.
                exec_in_erlang_default_shell(ConnectionHandler, ChannelId, Cmd, WantReply, S1);

            undefined ->
                %% Exec called, but the a shell other than the default shell is defined.
                %% No new exec shell is defined, so don't execute!
                %% We don't know if it is intended to use the new shell or not.
                {"Prohibited.", ?EXEC_ERROR_STATUS, 1};

            _ ->
                %% Exec called and a Fun or MFA is defined to use.  The F communicates via
                %% standard io:write/read.
                %% Kept for compatibility.
                S2 = start_exec_shell(ConnectionHandler, Cmd, S1),
                ssh_connection:reply_request(ConnectionHandler, WantReply, success, ChannelId),
                {ok, S2}
        end
    of
        {Reply, Status, Type} ->
            write_chars(ConnectionHandler, ChannelId, Type,
                        unicode:characters_to_binary(Reply, utf8, out_enc(S1))),
            ssh_connection:reply_request(ConnectionHandler, WantReply, success, ChannelId),
            ssh_connection:exit_status(ConnectionHandler, ChannelId, Status),
            ssh_connection:send_eof(ConnectionHandler, ChannelId),
            {stop, ChannelId, S1#state{channel = ChannelId, cm = ConnectionHandler}};
            
        {ok, S} ->
            {ok, S#state{channel = ChannelId,
                         cm = ConnectionHandler}}
    end;

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

handle_msg({Group, get_terminal_state}, State) ->
    Group ! {self(), get_terminal_state, true},
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
    {Chars0, NewBuf} = io_request(Req, Buf, Pty, Group),
    Chars = unicode:characters_to_binary(Chars0, utf8, out_enc(State)),
    write_chars(ConnectionHandler, ChannelId, Chars),
    {ok, State#state{buf = NewBuf}};

handle_msg({'EXIT', Group, Reason}, #state{group = Group,
					    cm = ConnectionHandler,
					    channel = ChannelId} = State) ->
    ssh_connection:send_eof(ConnectionHandler, ChannelId),
    ExitStatus = case Reason of
                     normal ->
                         0;
                     {exit_status, V} when is_integer(V) ->
                         V;
                     _ ->
                         ?EXEC_ERROR_STATUS
                 end,
    ssh_connection:exit_status(ConnectionHandler, ChannelId, ExitStatus),
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

claim_encoding(<<"/", _/binary>>) ->
    %% If the locale value begins with a slash, it is interpreted
    %% as the pathname of a file that was created in the output format
    %% used by the localedef utility; see OUTPUT FILES under localedef.
    %% Referencing such a pathname will result in that locale being used
    %% for the indicated category.
    undefined;

claim_encoding(EnvValue) ->
    %% If the locale value has the form:
    %%      language[_territory][.codeset]
    %% it refers to an implementation-provided locale, where settings of
    %% language, territory and codeset are implementation-dependent. 
    try string:tokens(binary_to_list(EnvValue), ".")
    of
        [_,"UTF-8"] -> {ok,utf8};
        [_,"ISO-8859-1"] -> {ok,latin1};  % There are -1 ... -16  called latin1..latin16
        _ -> undefined
    catch
        _:_ -> undefined
    end.


guess_encoding(Data0, #state{encoding = PeerEnc0,
                             deduced_encoding = TestEnc0} = State) ->
    Enc =
        case {PeerEnc0,TestEnc0} of
            {latin1,_} -> latin1;
            {_,latin1} -> latin1;
            _ -> case unicode:characters_to_binary(Data0, utf8, utf8) of
                     Data0 -> utf8;
                     _ -> latin1
                 end
        end,
    case TestEnc0 of
        Enc ->
            {Enc, State};
        latin1 ->
            {Enc, State};
        utf8 when Enc==latin1 ->
            {Enc, State#state{deduced_encoding=latin1}};
        undefined ->
            {Enc, State#state{deduced_encoding=Enc}}
    end.


out_enc(#state{encoding = PeerEnc,
               deduced_encoding = DeducedEnc}) ->
    case DeducedEnc of
        undefined -> PeerEnc;
        _ -> DeducedEnc
    end.

%%--------------------------------------------------------------------

to_group([], _Group, _Dumb) ->
    ok;
to_group([$\^C | Tail], Group, Dumb) ->
    exit(Group, interrupt),
    to_group(Tail, Group, Dumb);
to_group(Data, Group, Dumb) ->
    Func = fun(C) -> C /= $\^C end,
    Tail = case lists:splitwith(Func, Data) of
        {[], Right} ->
            Right;
        {Left, Right} ->
            %% Filter out escape sequences, only support Ctrl sequences
            Left1 = if Dumb -> replace_escapes(Left); true -> Left end,
            Group ! {self(), {data, Left1}},
            Right
    end,
    to_group(Tail, Group, Dumb).
replace_escapes(Data) ->
    lists:flatten([ if C =:= 27 ->
        [$^,C+64];
         true -> C
    end || C <- Data]).

%%--------------------------------------------------------------------
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
io_request({put_expand, unicode, Expand, _N}, Buf, Tty, _Group) ->
    insert_chars(unicode:characters_to_list("\n"++Expand, unicode), Buf, Tty);
io_request({insert_chars, Cs}, Buf, Tty, _Group) ->
    insert_chars(bin_to_list(Cs), Buf, Tty);
io_request({insert_chars, unicode, Cs}, Buf, Tty, _Group) ->
    insert_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({move_rel, N}, Buf, Tty, _Group) ->
    move_rel(N, Buf, Tty);
io_request({move_line, N}, Buf, Tty, _Group) ->
    move_line(N, Buf, Tty);
io_request({move_combo, L, V, R}, Buf, Tty, _Group) ->
    {ML, Buf1} = move_rel(L, Buf, Tty),
    {MV, Buf2} = move_line(V, Buf1, Tty),
    {MR, Buf3} = move_rel(R, Buf2, Tty),
    {[ML,MV,MR], Buf3};
io_request(new_prompt, _Buf, _Tty, _Group) ->
    {[], {[], {[],[]}, [], 0 }};
io_request(delete_line, {_, {_, _}, _, Col}, Tty, _Group) ->
    MoveToBeg = move_cursor(Col, 0, Tty),
    {[MoveToBeg, "\e[J"],
     {[],{[],[]},[],0}};
io_request({redraw_prompt, Pbs, Pbs2, {LB, {Bef, Aft}, LA}}, Buf, Tty, _Group) ->
    {ClearLine, Cleared} = io_request(delete_line, Buf, Tty, _Group),
    CL = lists:reverse(Bef,Aft),
    Text = Pbs ++ lists:flatten(lists:join("\n"++Pbs2, lists:reverse(LB)++[CL|LA])),
    Moves = if LA /= [] ->
                    [Last|_] = lists:reverse(LA),
                    {move_combo, -length(Last), -length(LA), length(Bef)};
               true ->
                    {move_rel, -length(Aft)}
            end,
    {T, InsertedText} = io_request({insert_chars, unicode:characters_to_binary(Text)}, Cleared, Tty, _Group),
    {M, Moved} = io_request(Moves, InsertedText, Tty, _Group),
    {[ClearLine, T, M], Moved};
io_request({delete_chars,N}, Buf, Tty, _Group) ->
    delete_chars(N, Buf, Tty);
io_request(clear, Buf, _Tty, _Group) ->
    {"\e[H\e[2J", Buf};
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

%% New in 18
io_request({put_chars_sync, Class, Cs, Reply}, Buf, Tty, Group) ->
    %% We handle these asynchronous for now, if we need output guarantees
    %% we have to handle these synchronously
    Group ! {reply, Reply, ok},
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
%% Note that Bef is reversed but Aft is not
%% (this is handy; the head is always next to the cursor)
conv_buf([], {LB, {Bef, Aft}, LA, Col}, AccWrite, _Tty) ->
    {{LB, {Bef, Aft}, LA, Col}, lists:reverse(AccWrite)};
conv_buf([13, 10 | Rest], {LB, {Bef, Aft}, LA, Col}, AccWrite, Tty = #ssh_pty{width = W}) ->
    conv_buf(Rest, {[lists:reverse(Bef)|LB], {[], tl2(Aft)}, LA, Col+(W-(Col rem W))}, [10, 13 | AccWrite], Tty);
conv_buf([13 | Rest], {LB, {Bef, Aft}, LA, Col}, AccWrite, Tty = #ssh_pty{width = W}) ->
    conv_buf(Rest, {[lists:reverse(Bef)|LB], {[], tl1(Aft)}, LA, Col+(W-(Col rem W))}, [13 | AccWrite], Tty);
conv_buf([10 | Rest],{LB, {Bef, Aft}, LA, Col}, AccWrite0, Tty = #ssh_pty{width = W}) ->
    AccWrite =
        case pty_opt(onlcr,Tty) of
            0 -> [10 | AccWrite0];
            1 -> [10,13 | AccWrite0];
            undefined -> [10 | AccWrite0]
        end,
    conv_buf(Rest, {[lists:reverse(Bef)|LB], {[], tl1(Aft)}, LA, Col+(W - (Col rem W))}, AccWrite, Tty);
conv_buf([C | Rest], {LB, {Bef, Aft}, LA, Col}, AccWrite, Tty) ->
    conv_buf(Rest, {LB, {[C|Bef], tl1(Aft)}, LA, Col+1}, [C | AccWrite], Tty).

%%% put characters before the prompt
put_chars(Chars, Buf, Tty) ->
    Dumb = get_dumb(Tty),
    case Buf of
        {[],{[],[]},[],_} -> {_, WriteBuf} = conv_buf(Chars, Buf, [], Tty),
            {WriteBuf, Buf};
        _ when Dumb =:= false ->
            {Delete, DeletedState} = io_request(delete_line, Buf, Tty, []),
            {_, PutBuffer} = conv_buf(Chars, DeletedState, [], Tty),
            {Redraw, _} = io_request(redraw_prompt_pre_deleted, Buf, Tty, []),
            {[Delete, PutBuffer, Redraw], Buf};
        _ ->
            %% When we have a dumb terminal, we get messages via put_chars requests
            %% so state should be empty {[],{[],[]},[],_},
            %% but if we end up here its not, so keep the state
            {_, WriteBuf} = conv_buf(Chars, Buf, [], Tty),
            {WriteBuf, Buf}
    end.

%%% insert character at current position
insert_chars([], Buf, _Tty) ->
    {[], Buf};
insert_chars(Chars, {_LB,{_Bef, Aft},LA, _Col}=Buf, Tty) ->
    {{NewLB, {NewBef, _NewAft}, _NewLA, NewCol}, WriteBuf} = conv_buf(Chars, Buf, [], Tty),
    M = move_cursor(special_at_width(NewCol+length(Aft), Tty), NewCol, Tty),
    {[WriteBuf, Aft | M], {NewLB,{NewBef, Aft},LA, NewCol}}.

%%% delete characters at current position, (backwards if negative argument)
delete_chars(0, {LB,{Bef, Aft},LA, Col}, _Tty) ->
    {[], {LB,{Bef, Aft},LA, Col}};
delete_chars(N, {LB,{Bef, Aft},LA, Col}, Tty) when N > 0 ->
    NewAft = nthtail(N, Aft),
    M = move_cursor(Col + length(NewAft) + N, Col, Tty),
    {[NewAft, lists:duplicate(N, $ ) | M],
     {LB,{Bef, NewAft},LA, Col}};
delete_chars(N, {LB,{Bef, Aft},LA, Col}, Tty) -> % N < 0
    NewBef = nthtail(-N, Bef),
    NewCol = case Col + N of V when V >= 0 -> V; _ -> 0 end,
    M1 = move_cursor(Col, NewCol, Tty),
    M2 = move_cursor(special_at_width(NewCol+length(Aft)-N, Tty), NewCol, Tty),
    {[M1, Aft, lists:duplicate(-N, $ ) | M2],
     {LB,{NewBef, Aft},LA, NewCol}}.

%%% Window change, redraw the current line (and clear out after it
%%% if current window is wider than previous)
window_change(Tty, OldTty, Buf)
  when OldTty#ssh_pty.width == Tty#ssh_pty.width ->
     %% No line width change
    {[], Buf};
window_change(Tty, OldTty, {LB, {Bef, Aft}, LA, Col}) ->
    case OldTty#ssh_pty.width - Tty#ssh_pty.width of
        0 ->
            %% No line width change
            {[], {LB, {Bef, Aft}, LA, Col}};

        DeltaW0 when DeltaW0 < 0,
                     Aft == [] ->
            % Line width is decreased, cursor is at end of input
            {[], {LB, {Bef, Aft}, LA, Col}};

        DeltaW0 when DeltaW0 < 0,
                     Aft =/= [] ->
            % Line width is decreased, cursor is not at end of input
            {[], {LB, {Bef, Aft}, LA, Col}};

        DeltaW0 when DeltaW0 > 0 ->
            % Line width is increased
            {[], {LB, {Bef, Aft}, LA, Col}}
        end.

%% move around in buffer, respecting pad characters
step_over(0, {LB, {Bef, [?PAD |Aft]}, LA, Col}) ->
    {LB, {[?PAD | Bef], Aft}, LA, Col+1};
step_over(0, {LB, {Bef, Aft}, LA, Col}) ->
    {LB, {Bef, Aft}, LA, Col};
step_over(N, {LB, {[C | Bef], Aft}, LA, Col}) when N < 0 ->
    N1 = ifelse(C == ?PAD, N, N+1),
    step_over(N1, {LB, {Bef, [C | Aft]}, LA, Col-1});
step_over(N, {LB, {Bef, [C | Aft]}, LA, Col}) when N > 0 ->
    N1 = ifelse(C == ?PAD, N, N-1),
    step_over(N1, {LB, {[C | Bef], Aft}, LA, Col+1}).

%%% an empty line buffer
empty_buf() -> {[], {[], []}, [], 0}.

%%% col and row from position with given width
col(N, W) -> N rem W.
row(N, W) -> N div W.

%%% move relative N characters
move_rel(N, {_LB, {_Bef, _Aft}, _LA, Col}=Buf, Tty) ->
    {NewLB, {NewBef, NewAft}, NewLA, NewCol} = step_over(N, Buf),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewLB, {NewBef, NewAft}, NewLA, NewCol}}.

move_line(V, {_LB, {_Bef, _Aft}, _LA, Col}, Tty = #ssh_pty{width=W})
        when V < 0, length(_LB) >= -V ->
    {LinesJumped, [B|NewLB]} = lists:split(-V -1, _LB),
    CL = lists:reverse(_Bef,_Aft),
    NewLA = lists:reverse([CL|LinesJumped], _LA),
    {NewBB, NewAft} = lists:split(min(length(_Bef),length(B)), B),
    NewBef = lists:reverse(NewBB),
    NewCol = Col - length(_Bef) - lists:sum([((length(L)-1) div W)*W + W || L <- [B|LinesJumped]]) + length(NewBB),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewLB, {NewBef, NewAft}, NewLA, NewCol}};
move_line(V, {_LB, {_Bef, _Aft}, _LA, Col}, Tty = #ssh_pty{width=W})
        when V > 0, length(_LA) >= V ->
    {LinesJumped, [A|NewLA]} = lists:split(V -1, _LA),
    CL = lists:reverse(_Bef,_Aft),
    NewLB = lists:reverse([CL|LinesJumped],_LB),
    {NewBB, NewAft} = lists:split(min(length(_Bef),length(A)), A),
    NewBef = lists:reverse(NewBB),
    NewCol = Col - length(_Bef) + lists:sum([((length(L)-1) div W)*W + W || L <- [CL|LinesJumped]]) + length(NewBB),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewLB, {NewBef, NewAft}, NewLA, NewCol}};
move_line(_, Buf, _) ->
    {"", Buf}.
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

%%% Caution for line "breaks"
special_at_width(From0, #ssh_pty{width=Width}) when (From0 rem Width) == 0 -> From0 - 1;
special_at_width(From0, _) -> From0.

%% %%% write out characters
%% %%% make sure that there is data to send
%% %%% before calling ssh_connection:send
write_chars(ConnectionHandler, ChannelId, Chars) ->
    write_chars(ConnectionHandler, ChannelId, ?SSH_EXTENDED_DATA_DEFAULT, Chars).

write_chars(ConnectionHandler, ChannelId, Type, Chars) ->
    case has_chars(Chars) of
        false -> ok;
        true -> ssh_connection:send(ConnectionHandler,
                                    ChannelId,
                                    Type,
                                    Chars)
    end.

has_chars([C|_]) when is_integer(C) -> true;
has_chars([H|T]) when is_list(H) ; is_binary(H) -> has_chars(H) orelse has_chars(T);
has_chars(<<_:8,_/binary>>) -> true;
has_chars(_) -> false.
    

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


%%--------------------------------------------------------------------
start_shell(ConnectionHandler, State) ->
    ShellSpawner =
        case State#state.shell of
            Shell when is_function(Shell, 1) ->
                [{user,User}] = ssh_connection_handler:connection_info(ConnectionHandler, [user]),
                fun() -> Shell(User) end;
            Shell when is_function(Shell, 2) ->
                ConnectionInfo =
                    ssh_connection_handler:connection_info(ConnectionHandler, [peer, user]),
                User = proplists:get_value(user, ConnectionInfo),
                {_, PeerAddr} = proplists:get_value(peer, ConnectionInfo),
                fun() -> Shell(User, PeerAddr) end;
            {_,_,_} = Shell ->
                Shell
        end,
    State#state{group = group:start(self(), ShellSpawner,
                                    [{dumb, get_dumb(State#state.pty)},{expand_below, false},
                                     {echo, get_echo(State#state.pty)}]),
                buf = empty_buf()}.

%%--------------------------------------------------------------------
start_exec_shell(ConnectionHandler, Cmd, State) ->
    ExecShellSpawner =
        case State#state.exec of
            ExecShell when is_function(ExecShell, 1) ->
                fun() -> ExecShell(Cmd) end;
            ExecShell when is_function(ExecShell, 2) ->
                [{user,User}] = ssh_connection_handler:connection_info(ConnectionHandler, [user]),
                fun() -> ExecShell(Cmd, User) end;
            ExecShell when is_function(ExecShell, 3) ->
                ConnectionInfo =
                    ssh_connection_handler:connection_info(ConnectionHandler, [peer, user]),
                User = proplists:get_value(user, ConnectionInfo),
                {_, PeerAddr} = proplists:get_value(peer, ConnectionInfo),
                fun() -> ExecShell(Cmd, User, PeerAddr) end;
            {M,F,A} ->
                {M, F, A++[Cmd]}
        end,
    State#state{group = group:start(self(), ExecShellSpawner, [{expand_below, false},
                                                               {echo,false}]),
                buf = empty_buf()}.

%%--------------------------------------------------------------------
exec_in_erlang_default_shell(ConnectionHandler, ChannelId, Cmd, WantReply, State) ->
    exec_in_self_group(ConnectionHandler, ChannelId, WantReply, State,
      fun() ->
              eval(parse(scan(Cmd)))
      end).


scan(Cmd) ->
    erl_scan:string(Cmd). 

parse({ok, Tokens, _}) ->
    erl_parse:parse_exprs(Tokens);
parse({error, {_,erl_scan,Cause}, _}) ->
    {error, erl_scan:format_error(Cause)}.

eval({ok, Expr_list}) ->
    {value, Value, _NewBindings} = erl_eval:exprs(Expr_list, erl_eval:new_bindings()),
    {ok, Value};
eval({error, {_,erl_parse,Cause}}) ->
    {error, erl_parse:format_error(Cause)};
eval({error,Error}) ->
    {error, Error}.

%%--------------------------------------------------------------------
exec_direct(ConnectionHandler, ChannelId, Cmd, ExecSpec, WantReply, State) ->
    Fun =
        fun() ->
                if
                    is_function(ExecSpec, 1) ->
                        ExecSpec(Cmd);

                    is_function(ExecSpec, 2) ->
                        [{user,User}] = ssh_connection_handler:connection_info(ConnectionHandler, [user]),
                        ExecSpec(Cmd, User);

                    is_function(ExecSpec, 3) ->
                        ConnectionInfo =
                            ssh_connection_handler:connection_info(ConnectionHandler, [peer, user]),
                        User = proplists:get_value(user, ConnectionInfo),
                        {_, PeerAddr} = proplists:get_value(peer, ConnectionInfo),
                        ExecSpec(Cmd, User, PeerAddr);

                    true ->
                        {error, "Bad exec fun in server"}
                end
        end,
    exec_in_self_group(ConnectionHandler, ChannelId, WantReply, State, Fun).

%%--------------------------------------------------------------------
%% Help for directing stdin and stdout from and to the channel from/to the client
%%
exec_in_self_group(ConnectionHandler, ChannelId, WantReply, State, Fun) ->
    Exec =
        fun() ->
                spawn(
                  fun() ->
                          case try
                                   ssh_connection:reply_request(ConnectionHandler, WantReply, success, ChannelId),
                                   Fun()
                               of
                                   {ok, Result} ->
                                       {ok, Result};
                                   {error, Error} ->
                                       {error, Error};
                                   X ->
                                       {error, "Bad exec fun in server. Invalid return value: "++t2str(X)}
                               catch error:Err ->
                                       {error,Err};
                                     Cls:Exp ->
                                       {error,{Cls,Exp}}
                               end
                          of
                              {ok,Str} ->
                                  write_chars(ConnectionHandler, ChannelId, t2str(Str));
                              {error, Str} ->
                                  write_chars(ConnectionHandler, ChannelId, 1, "**Error** "++t2str(Str)),
                                  exit({exit_status, ?EXEC_ERROR_STATUS})
                          end
                  end)
        end,
    {ok, State#state{group = group:start(self(), Exec, [{expand_below, false},
                                                        {echo,false}]),
                     buf = empty_buf()}}.
    

t2str(T) -> try io_lib:format("~s",[T])
            catch _:_ -> io_lib:format("~p",[T])
            end.

%%--------------------------------------------------------------------
get_dumb(Tty) ->
    try
        Tty#ssh_pty.term =:= "dumb"
    catch
        _:_ -> false
    end.

% Pty can be undefined if the client never sets any pty options before
% starting the shell.
get_echo(Tty) ->
    case pty_opt(echo,Tty) of
        0 -> false;
        1 -> true;
        undefined -> true
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

%%%----------------------------------------------------------------
pty_opt(Name, Tty) ->
    try
        proplists:get_value(Name, Tty#ssh_pty.modes, undefined)
    catch
        _:_ -> undefined
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [terminate, cli, cli_details].

ssh_dbg_flags(cli) -> [c];
ssh_dbg_flags(terminate) -> [c].

ssh_dbg_on(cli) -> dbg:tp(?MODULE,handle_ssh_msg,2,x),
                   dbg:tp(?MODULE,write_chars,4,x);
ssh_dbg_on(cli_details) -> dbg:tp(?MODULE,handle_msg,2,x);
ssh_dbg_on(terminate) -> dbg:tp(?MODULE,  terminate, 2, x).


ssh_dbg_off(cli) -> dbg:ctpg(?MODULE,handle_ssh_msg,2),
                    dbg:ctpg(?MODULE,write_chars,4);
ssh_dbg_off(cli_details) -> dbg:ctpg(?MODULE,handle_msg,2);
ssh_dbg_off(terminate) -> dbg:ctpg(?MODULE, terminate, 2).


ssh_dbg_format(cli, {call,{?MODULE,handle_ssh_msg,
                           [{ssh_cm, _ConnectionHandler, Request},
                            S = #state{channel=Ch}]}}) when is_tuple(Request) ->
    [io_lib:format("CLI conn ~p chan ~p, req ~p", 
                   [self(),Ch,element(1,Request)]),
     case Request of
         {window_change, ChannelId, Width, Height, PixWidth, PixHeight} ->
             fmt_kv([{channel_id,ChannelId}, 
                     {width,Width}, {height,Height},
                     {pix_width,PixWidth}, {pixel_hight,PixHeight}]);

         {env, ChannelId, WantReply, Var, Value} ->
             fmt_kv([{channel_id,ChannelId}, {want_reply,WantReply}, {Var,Value}]);

         {exec, ChannelId, WantReply, Cmd} ->
             fmt_kv([{channel_id,ChannelId}, {want_reply,WantReply}, {command,Cmd}]);

         {pty, ChannelId, WantReply,
          {TermName, Width, Height, PixWidth, PixHeight, Modes}} ->
             fmt_kv([{channel_id,ChannelId}, {want_reply,WantReply}, 
                     {term,TermName},
                     {width,Width}, {height,Height}, {pix_width,PixWidth}, {pixel_hight,PixHeight},
                     {pty_opts, Modes}]);

         {data, ChannelId, Type, Data} -> 
             fmt_kv([{channel_id,ChannelId},
                     {type, type(Type)},
                     {data, us, ssh_dbg:shrink_bin(Data)},
                     {hex, h, Data}
                    ]);

         {shell, ChannelId, WantReply} ->
             fmt_kv([{channel_id,ChannelId},
                     {want_reply,WantReply},
                     {encoding, S#state.encoding},
                     {pty, S#state.pty}
                    ]);

         _ ->
             io_lib:format("~nunder construction:~nRequest = ~p",[Request])
     end];
ssh_dbg_format(cli, {call,{?MODULE,handle_ssh_msg,_}}) -> skip;
ssh_dbg_format(cli, {return_from,{?MODULE,handle_ssh_msg,2},_Result}) -> skip;

ssh_dbg_format(cli, {call,{?MODULE,write_chars, [C, Ch, Type, Chars]}}) ->
    [io_lib:format("CLI conn ~p chan ~p reply", [C,Ch]),
     fmt_kv([{channel_id,Ch},
             {type, type(Type)},
             {data, us, ssh_dbg:shrink_bin(Chars)},
             {hex, h, Chars}
            ])];
ssh_dbg_format(cli, {return_from,{?MODULE,write_chars,4},_Result}) -> skip;


ssh_dbg_format(cli_details, {call,{?MODULE,handle_msg,
                                   [{Group,Arg}, #state{channel=Ch}]}}) ->
    [io_lib:format("CLI detail conn ~p chan ~p group ~p",
                   ['?', Ch, Group]),
     case Arg of
         {put_chars_sync,Class,Cs,Reply} ->
             fmt_kv([{op, put_chars_sync},
                     {class, Class},
                     {data, us, ssh_dbg:shrink_bin(Cs)},
                     {hex, h, Cs},
                     {reply, Reply}]);
         _ ->
             io_lib:format("~nunder construction:~nRequest = ~p",[Arg])
     end];
ssh_dbg_format(cli_details, {call,{?MODULE,handle_msg,_}}) -> skip;
ssh_dbg_format(cli_details, {return_from,{?MODULE,handle_msg,2},_Result}) -> skip;

ssh_dbg_format(terminate, {call, {?MODULE,terminate, [Reason, State]}}) ->
    ["Cli Terminating:\n",
     io_lib:format("Reason: ~p,~nState:~n~s", [Reason, wr_record(State)])
    ];
ssh_dbg_format(terminate, {return_from, {?MODULE,terminate,2}, _Ret}) ->
    skip.


?wr_record(state).

fmt_kv(KVs) -> lists:map(fun fmt_kv1/1, KVs).

fmt_kv1({K,V})   -> io_lib:format("~n~p: ~p",[K,V]);
fmt_kv1({K,s,V}) -> io_lib:format("~n~p: ~s",[K,V]);
fmt_kv1({K,us,V}) -> io_lib:format("~n~p: ~ts",[K,V]);
fmt_kv1({K,h,V}) -> io_lib:format("~n~p: ~s",[K, [$\n|ssh_dbg:hex_dump(V)]]).

type(0) -> "0 (normal data)";
type(1) -> "1 (extended data, i.e. errors)";
type(T) -> T.
