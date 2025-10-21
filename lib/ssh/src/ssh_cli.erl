%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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
	  shell,
	  exec,
      tty

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
    TTY = prim_tty:init_ssh(#{input => false}, {80, 24}, utf8),
    {ok, #state{shell = Shell, exec = Exec, tty = TTY}};
init([Shell]) ->
    TTY = prim_tty:init_ssh(#{input => false}, {80, 24}, utf8),
    {ok, #state{shell = Shell, tty = TTY}}.

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
                  modes = Modes}},
    TTY = prim_tty:init_ssh(#{input => false}, {not_zero(Width, 80), not_zero(Height, 24)}, utf8),
    set_echo(State),
    ssh_connection:reply_request(ConnectionHandler, WantReply,
				 success, ChannelId),
    {ok, State#state{tty = TTY}};

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
    NewTTY = prim_tty:unicode(State#state.tty, Enc =:= utf8),
    {ok, State#state{encoding=Enc, tty = NewTTY}};

handle_ssh_msg({ssh_cm, ConnectionHandler,
	    {window_change, ChannelId, Width, Height, PixWidth, PixHeight}},
	   #state{pty = Pty0, tty = TTY} = State) ->
    Pty = Pty0#ssh_pty{width = Width, height = Height,
		       pixel_width = PixWidth,
		       pixel_height = PixHeight},
    NewTTY = prim_tty:update_geometry(TTY, Width, Height),
    {Chars, NewTTY1} = io_request(redraw_prompt, NewTTY, State),
    write_chars(ConnectionHandler, ChannelId, Chars),
    {ok, State#state{pty = Pty, tty = NewTTY1}};

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
    Group ! {self(), get_unicode_state, State#state.encoding =:= utf8},
    {ok, State};

handle_msg({Group, get_terminal_state}, State) ->
    Group ! {self(), get_terminal_state, #{ stdout => true, stdin => true }},
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
handle_msg({Group, {open_editor, _}}, State) ->
    %% Opening an external editor in ssh is not supported.
    Group ! {self(), not_supported},
    {ok,State};
handle_msg({Group, Req}, #state{group = Group,cm = ConnectionHandler,
                                channel = ChannelId, tty = TTY} = State) ->
    {Chars0, NewTTY} = io_request(Req, TTY, State),
    Chars = unicode:characters_to_binary(Chars0, utf8, out_enc(State)),
    write_chars(ConnectionHandler, ChannelId, Chars),
    {ok, State#state{tty = NewTTY}};

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
%%% 
%%-spec io_request(term(), prim_tty:state(), state()) -> {list(), prim_tty:state()}.
io_request({requests,Rs}, TTY, State) ->
    io_requests(Rs, TTY, State, []);
io_request(redraw_prompt, TTY, _State) ->
    prim_tty:handle_request(TTY, redraw_prompt);
io_request({redraw_prompt, Pbs, Pbs2, LineState}, TTY, _State) ->
    prim_tty:handle_request(TTY, {redraw_prompt, Pbs, Pbs2, LineState});
io_request(new_prompt, TTY, _State) ->
    prim_tty:handle_request(TTY, new_prompt);
io_request(delete_after_cursor, TTY, _State) ->
    prim_tty:handle_request(TTY, delete_after_cursor);
io_request(delete_line, TTY, _State) ->
    prim_tty:handle_request(TTY, delete_line);
io_request({put_chars, unicode, Chars}, TTY, _State) ->
    prim_tty:handle_request(TTY, {putc, unicode:characters_to_binary(Chars)});
io_request({put_chars_sync, unicode, Chars, Reply}, TTY, State) ->
    State#state.group ! {reply, Reply, ok},
    prim_tty:handle_request(TTY, {putc, unicode:characters_to_binary(Chars)});
io_request({put_expand, unicode, Chars, N}, TTY, _State) ->
    prim_tty:handle_request(TTY, {expand, unicode:characters_to_binary(Chars), N});
io_request({move_expand, N}, TTY, _State) ->
    prim_tty:handle_request(TTY, {move_expand, N});
io_request({move_rel, N}, TTY, _State) ->
    prim_tty:handle_request(TTY, {move, N});
io_request({move_line, R}, TTY, _State) ->
    prim_tty:handle_request(TTY, {move_line, R});
io_request({move_combo, V1, R, V2}, TTY, _State) ->
    prim_tty:handle_request(TTY, {move_combo, V1, R, V2});
io_request({insert_chars, unicode, Chars}, TTY, _State) ->
    prim_tty:handle_request(TTY, {insert, unicode:characters_to_binary(Chars)});
io_request({insert_chars_over, unicode, Chars}, TTY, _State) ->
    prim_tty:handle_request(TTY, {insert_over, unicode:characters_to_binary(Chars)});
io_request({delete_chars, N}, TTY, _State) ->
    prim_tty:handle_request(TTY, {delete, N});
io_request(clear, TTY, _State) ->
    prim_tty:handle_request(TTY, clear);
io_request(beep, TTY, _State) ->
    prim_tty:handle_request(TTY, beep);
io_request(Req, TTY, _State) ->
    erlang:display({unhandled_request, Req}),
    {[], TTY}.
io_requests([{insert_chars, unicode, C1},{insert_chars, unicode, C2}|Rs], TTY, State, Acc) ->
    io_requests([{insert_chars, unicode, [C1,C2]}|Rs], TTY, State, Acc);
io_requests([{put_chars, unicode, C1},{put_chars, unicode, C2}|Rs], TTY, State, Acc) ->
    io_requests([{put_chars, unicode, [C1,C2]}|Rs], TTY, State, Acc);
io_requests([{move_rel, N}, {move_line, R}, {move_rel, M}|Rs], TTY, State, Acc) ->
    io_requests([{move_combo, N, R, M}|Rs], TTY, State, Acc);
io_requests([{move_rel, N}, {move_line, R}|Rs], TTY, State, Acc) ->
    io_requests([{move_combo, N, R, 0}|Rs], TTY, State, Acc);
io_requests([{move_line, R}, {move_rel, M}|Rs], TTY, State, Acc) ->
    io_requests([{move_combo, 0, R, M}|Rs], TTY, State, Acc);
io_requests([R|Rs], TTY, State, Acc) ->
    {Chars, NewTTY} = io_request(R, TTY, State),
    io_requests(Rs, NewTTY, State, [Chars|Acc]);
io_requests([], TTY, _State, Acc) ->
    {lists:reverse(Acc), TTY}.

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
                                    [{dumb, get_dumb(State#state.pty)},
                                     {expand_below, application:get_env(stdlib, shell_expand_location, below) =:= below},
                                     {echo, get_echo(State#state.pty)}])}.

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
                                                               {dumb, true}])}.

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
                                                        {dumb, true}])}}.
    

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
