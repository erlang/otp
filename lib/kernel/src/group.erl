%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(group).
-moduledoc false.

-include_lib("kernel/include/logger.hrl").

%% A group leader process for user io.
%% This process receives input data from user_drv in this format
%%   {Drv,{data,unicode:charlist()}}
%% It then keeps that data as unicode in its state and converts it
%% to latin1/unicode on a per request basis. If any data is left after
%% a request, that data is again kept as unicode.

-export([start/2, start/3, whereis_shell/0, server/4]).

-export([server_loop/3]).

start(Drv, Shell) ->
    start(Drv, Shell, []).

start(Drv, Shell, Options) ->
    Ancestors = [self() | case get('$ancestors') of
                              undefined -> [];
                              Anc -> Anc
                          end],
    spawn_link(group, server, [Ancestors, Drv, Shell, Options]).

server(Ancestors, Drv, Shell, Options) ->
    process_flag(trap_exit, true),
    _ = [put('$ancestors', Ancestors) || Shell =/= {}],
    edlin:init(),
    put(read_mode, list),
    put(user_drv, Drv),

    ExpandFun = normalize_expand_fun(Options, fun edlin_expand:expand/2),
    put(expand_fun, ExpandFun),

    %% echo can be set to false by -oldshell and ssh_cli
    put(echo, proplists:get_value(echo, Options, true)),

    %% dumb can be set to true by ssh_cli
    put(dumb, proplists:get_value(dumb, Options, false)),

    %% noshell can be set to true by user_drv
    put(noshell, proplists:get_value(noshell, Options, false)),

    %% expand_below can be set by user_drv and ssh_cli
    put(expand_below, proplists:get_value(expand_below, Options, true)),

    DefaultGroupHistory =
        case not get(echo) of
            true ->
                [];
            false ->
                group_history:load()
        end,

    put(line_buffer, proplists:get_value(line_buffer, Options, DefaultGroupHistory)),

    server_loop(Drv, start_shell(Shell), []).

whereis_shell() ->
    case node(group_leader()) of
        Node when Node =:= node() ->
            case user_drv:whereis_group() of
                undefined -> undefined;
                GroupPid ->
                    {dictionary, Dict} = erlang:process_info(GroupPid, dictionary),
                    proplists:get_value(shell, Dict)
            end;
        OtherNode ->
            erpc:call(OtherNode, group, whereis_shell, [])
    end.

%% start_shell(Shell)
%%  Spawn a shell with its group_leader from the beginning set to ourselves.
%%  If Shell a pid the set its group_leader.

start_shell({Mod,Func,Args}) ->
    start_shell1(Mod, Func, Args);
start_shell({Node,Mod,Func,Args}) ->
    start_shell1(rpc, call, [Node,Mod,Func,Args]);
start_shell(Shell) when is_atom(Shell) ->
    start_shell1(Shell, start, []);
start_shell(Shell) when is_function(Shell) ->
    start_shell1(Shell);
start_shell(Shell) when is_pid(Shell) ->
    group_leader(self(), Shell),		% we are the shells group leader
    link(Shell),				% we're linked to it.
    put(shell, Shell),
    Shell;
start_shell(_Shell) ->
    ok.

start_shell1(M, F, Args) ->
    G = group_leader(),
    group_leader(self(), self()),
    case catch apply(M, F, Args) of
	Shell when is_pid(Shell) ->
	    group_leader(G, self()),
	    link(Shell),			% we're linked to it.
	    put(shell, Shell),
            Shell;
	Error ->				% start failure
	    exit(Error)				% let the group process crash

    end.

start_shell1(Fun) ->
    G = group_leader(),
    group_leader(self(), self()),
    case catch Fun() of
	Shell when is_pid(Shell) ->
	    group_leader(G, self()),
	    link(Shell),			% we're linked to it.
	    put(shell, Shell),
            Shell;
	Error ->				% start failure
	    exit(Error)				% let the group process crash
    end.

-spec server_loop(UserDrv :: pid(), Shell:: pid(),
                  Buffer :: unicode:chardata()) ->
          no_return().
server_loop(Drv, Shell, Buf0) ->
    receive
        {io_request,From,ReplyAs,Req} when is_pid(From) ->
            %% This io_request may cause a transition to a couple of
            %% selective receive loops elsewhere in this module.
            Buf = io_request(Req, From, ReplyAs, Drv, Shell, Buf0),
            ?MODULE:server_loop(Drv, Shell, Buf);
        {reply,{From,ReplyAs},Reply} ->
            io_reply(From, ReplyAs, Reply),
	    ?MODULE:server_loop(Drv, Shell, Buf0);
	{driver_id,ReplyTo} ->
	    ReplyTo ! {self(),driver_id,Drv},
	    ?MODULE:server_loop(Drv, Shell, Buf0);
	{Drv, echo, Bool} ->
	    put(echo, Bool),
	    ?MODULE:server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,interrupt} ->
	    %% Send interrupt to the shell.
	    exit_shell(interrupt),
	    ?MODULE:server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,R} ->
	    exit(R);
	{'EXIT',Shell,R} ->
	    exit(R);
	%% We want to throw away any term that we don't handle (standard
	%% practice in receive loops), but not any {Drv,_} tuples which are
	%% handled in io_request/6.
	NotDrvTuple when (not is_tuple(NotDrvTuple)) orelse
			 (tuple_size(NotDrvTuple) =/= 2) orelse
			 (element(1, NotDrvTuple) =/= Drv) ->
	    %% Ignore this unknown message.
	    ?MODULE:server_loop(Drv, Shell, Buf0)
    end.

exit_shell(Reason) ->
    case get(shell) of
	undefined -> true;
	Pid -> exit(Pid, Reason)
    end.

get_tty_geometry(Drv) ->
    Drv ! {self(),tty_geometry},
    receive
        {Drv,tty_geometry,Geometry} ->
            Geometry
    after 2000 ->
            timeout
    end.
get_unicode_state(Drv) ->
    Drv ! {self(),get_unicode_state},
    receive
        {Drv,get_unicode_state,UniState} ->
            UniState;
        {Drv,get_unicode_state,error} ->
            {error, internal}
    after 2000 ->
            {error,timeout}
    end.
set_unicode_state(Drv,Bool) ->
    Drv ! {self(),set_unicode_state,Bool},
    receive
        {Drv,set_unicode_state,_OldUniState} ->
            ok
    after 2000 ->
            timeout
    end.

get_terminal_state(Drv) ->
    Drv ! {self(),get_terminal_state},
    receive
	{Drv,get_terminal_state,Terminal} ->
	    Terminal;
	{Drv,get_terminal_state,error} ->
	    {error, internal}
    after 2000 ->
	    {error,timeout}
    end.

io_request(Req, From, ReplyAs, Drv, Shell, Buf0) ->
    case io_request(Req, Drv, Shell, {From,ReplyAs}, Buf0) of
	{ok,Reply,Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
        {noreply,Buf} ->
            %% We expect a {reply,_} message from the Drv when request is done
            Buf;
	{error,Reply,Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{exit,R} ->
	    %% 'kill' instead of R, since the shell is not always in
	    %% a state where it is ready to handle a termination
	    %% message.
	    exit_shell(kill),
	    exit(R)
    end.


%% Put_chars, unicode is the normal message, characters are always in
%% standard unicode format.
%% You might be tempted to send binaries unchecked, but the driver
%% expects unicode, so that is what we should send...
%% io_request({put_chars,unicode,Binary}, Drv, Buf) when is_binary(Binary) ->
%%     send_drv(Drv, {put_chars,Binary}),
%%     {ok,ok,Buf};
%%
%% These put requests have to be synchronous to the driver as otherwise
%% there is no guarantee that the data has actually been printed.
io_request({put_chars,unicode,Chars}, Drv, _Shell, From, Buf) ->
    case catch unicode:characters_to_binary(Chars,utf8) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode, Binary, From}),
	    {noreply,Buf};
	_ ->
	    {error,{error,{put_chars, unicode,Chars}},Buf}
    end;
io_request({put_chars,unicode,M,F,As}, Drv, _Shell, From, Buf) ->
    case catch apply(M, F, As) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode, Binary, From}),
	    {noreply,Buf};
	Chars ->
	    case catch unicode:characters_to_binary(Chars,utf8) of
		B when is_binary(B) ->
		    send_drv(Drv, {put_chars_sync, unicode, B, From}),
		    {noreply,Buf};
		_ ->
		    {error,{error,F},Buf}
	    end
    end;
io_request({put_chars,latin1,Binary}, Drv, _Shell, From, Buf) when is_binary(Binary) ->
    send_drv(Drv, {put_chars_sync, unicode,
                   unicode:characters_to_binary(Binary,latin1),
                   From}),
    {noreply,Buf};
io_request({put_chars,latin1,Chars}, Drv, _Shell, From, Buf) ->
    case catch unicode:characters_to_binary(Chars,latin1) of
        Binary when is_binary(Binary) ->
            send_drv(Drv, {put_chars_sync, unicode, Binary, From}),
            {noreply,Buf};
        _ ->
            {error,{error,{put_chars,latin1,Chars}},Buf}
    end;
io_request({put_chars,latin1,M,F,As}, Drv, _Shell, From, Buf) ->
    case catch apply(M, F, As) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode,
                           unicode:characters_to_binary(Binary,latin1),
                           From}),
	    {noreply,Buf};
	Chars ->
	    case catch unicode:characters_to_binary(Chars,latin1) of
		B when is_binary(B) ->
		    send_drv(Drv, {put_chars_sync, unicode, B, From}),
		    {noreply,Buf};
		_ ->
		    {error,{error,F},Buf}
	    end
    end;

io_request({get_chars,Encoding,Prompt,N}, Drv, Shell, _From, Buf) ->
    get_chars_n(Prompt, io_lib, collect_chars, N, Drv, Shell, Buf, Encoding);
io_request({get_line,Encoding,Prompt}, Drv, Shell, _From, Buf) ->
    get_chars_line(Prompt, io_lib, collect_line, [], Drv, Shell, Buf, Encoding);
io_request({get_until,Encoding, Prompt,M,F,As}, Drv, Shell, _From, Buf) ->
    get_chars_line(Prompt, io_lib, get_until, {M,F,As}, Drv, Shell, Buf, Encoding);
io_request({get_password,_Encoding},Drv,Shell,_From,Buf) ->
    get_password_chars(Drv, Shell, Buf);
io_request({setopts,Opts}, Drv, _Shell, _From, Buf) when is_list(Opts) ->
    setopts(Opts, Drv, Buf);
io_request(getopts, Drv, _Shell, _From, Buf) ->
    getopts(Drv, Buf);
io_request({requests,Reqs}, Drv, Shell, From, Buf) ->
    io_requests(Reqs, {ok,ok,Buf}, From, Drv, Shell);

%% New in R12
io_request({get_geometry,columns},Drv,_Shell,_From,Buf) ->
    case get_tty_geometry(Drv) of
	{W,_H} ->
	    {ok,W,Buf};
	_ ->
	    {error,{error,enotsup},Buf}
    end;
io_request({get_geometry,rows},Drv,_Shell,_From,Buf) ->
    case get_tty_geometry(Drv) of
	{_W,H} ->
	    {ok,H,Buf};
	_ ->
	    {error,{error,enotsup},Buf}
    end;

%% BC with pre-R13
io_request({put_chars,Chars}, Drv, Shell, From, Buf) ->
    io_request({put_chars,latin1,Chars}, Drv, Shell, From, Buf);
io_request({put_chars,M,F,As}, Drv, Shell, From, Buf) ->
    io_request({put_chars,latin1,M,F,As}, Drv, Shell, From, Buf);
io_request({get_chars,Prompt,N}, Drv, Shell, From, Buf) ->
    io_request({get_chars,latin1,Prompt,N}, Drv, Shell, From, Buf);
io_request({get_line,Prompt}, Drv, Shell, From, Buf) ->
    io_request({get_line,latin1,Prompt}, Drv, Shell, From, Buf);
io_request({get_until, Prompt,M,F,As}, Drv, Shell, From, Buf) ->
    io_request({get_until,latin1, Prompt,M,F,As}, Drv, Shell, From, Buf);
io_request(get_password,Drv,Shell,From,Buf) ->
    io_request({get_password,latin1},Drv,Shell,From,Buf);



io_request(_, _Drv, _Shell, _From, Buf) ->
    {error,{error,request},Buf}.

%% Status = io_requests(RequestList, PrevStat, From, Drv, Shell)
%%  Process a list of output requests as long as
%%  the previous status is 'ok' or noreply.
%%
%%  We use undefined as the From for all but the last request
%%  in order to discards acknowledgements from those requests.
%%
io_requests([R|Rs], {noreply,Buf}, From, Drv, Shell) ->
    ReqFrom = if Rs =:= [] -> From; true -> undefined end,
    io_requests(Rs, io_request(R, Drv, Shell, ReqFrom, Buf), From, Drv, Shell);
io_requests([R|Rs], {ok,ok,Buf}, From, Drv, Shell) ->
    ReqFrom = if Rs =:= [] -> From; true -> undefined end,
    io_requests(Rs, io_request(R, Drv, Shell, ReqFrom, Buf), From, Drv, Shell);
io_requests([_|_], Error, _From, _Drv, _Shell) ->
    Error;
io_requests([], Stat, _From, _, _Shell) ->
    Stat.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(undefined, _ReplyAs, _Reply) ->
    %% Ignore these replies as they are generated from io_requests/5.
    ok;
io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply},
    ok.

%% send_drv(Drv, Message)
%% send_drv_reqs(Drv, Requests)

send_drv(Drv, Msg) ->
    Drv ! {self(),Msg},
    ok.

send_drv_reqs(_Drv, []) -> ok;
send_drv_reqs(Drv, Rs) ->
    send_drv(Drv, {requests,Rs}).

expand_encoding([]) ->
    [];
expand_encoding([latin1 | T]) ->
    [{encoding,latin1} | expand_encoding(T)];
expand_encoding([unicode | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([H|T]) ->
    [H|expand_encoding(T)].
%% setopts
setopts(Opts0,Drv,Buf) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{list,binary}], 
	       expand_encoding(Opts0))),
    case check_valid_opts(Opts) of
	true ->
	    do_setopts(Opts,Drv,Buf);
	false ->
	    {error,{error,enotsup},Buf}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,Flag}|T]) when is_boolean(Flag) ->
    check_valid_opts(T);
check_valid_opts([{encoding,Valid}|T]) when Valid =:= unicode;
                                            Valid =:= utf8;
                                            Valid =:= latin1 ->
    check_valid_opts(T);
check_valid_opts([{echo,Flag}|T]) when is_boolean(Flag) ->
    check_valid_opts(T);
check_valid_opts([{expand_fun,Fun}|T]) when is_function(Fun, 1);
                                            is_function(Fun, 2) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

do_setopts(Opts, Drv, Buf) ->
    put(expand_fun, normalize_expand_fun(Opts, get(expand_fun))),
    put(echo, proplists:get_value(echo, Opts, get(echo))),
    case proplists:get_value(encoding, Opts) of
	Valid when Valid =:= unicode; Valid =:= utf8 ->
           set_unicode_state(Drv,true);
	latin1 ->
           set_unicode_state(Drv,false);
	undefined ->
	    ok
    end,
    case proplists:get_value(binary, Opts, case get(read_mode) of
					      binary -> true;
					      _ -> false
					   end) of
	true ->
	    put(read_mode, binary),
	    {ok,ok,Buf};
	false ->
	    put(read_mode, list),
	    {ok,ok,Buf}
    end.

normalize_expand_fun(Options, Default) ->
    case proplists:get_value(expand_fun, Options, Default) of
	Fun when is_function(Fun, 1) -> fun(X,_) -> Fun(X) end;
	Fun -> Fun
    end.

getopts(Drv,Buf) ->
    Exp = {expand_fun, case get(expand_fun) of
			   Func when is_function(Func) ->
			       Func;
			   _ ->
			       false
		       end},
    Echo = {echo, case get(echo) of
		     Bool when Bool =:= true; Bool =:= false ->
			 Bool;
		     _ ->
			 false
		  end},
    Bin = {binary, case get(read_mode) of
		       binary ->
			   true;
		       _ ->
			   false
		   end},
    Uni = {encoding, case get_unicode_state(Drv) of
			true -> unicode;
			_ -> latin1
		     end},
    Terminal = get_terminal_state(Drv),
    Tty = {terminal, maps:get(stdout, Terminal)},
    {ok,[Exp,Echo,Bin,Uni,Tty|maps:to_list(Terminal)],Buf}.

%% get_chars_*(Prompt, Module, Function, XtraArgument, Drv, Buffer)
%%  Gets characters from the input Drv until as the applied function
%%  returns {stop,Result,Rest}. Does not block output until input has been
%%  received.
%%  Returns:
%%	{Result,NewSaveBuffer}
%%	{error,What,NewSaveBuffer}

get_password_chars(Drv,Shell,Buf) ->
    case get(echo) of
        true ->
            case get_password_line(Buf, Drv, Shell) of
                {done, Line, Buf1} ->
                    {ok, Line, Buf1};
                interrupted ->
                    {error, {error, interrupted}, []};
                terminated ->
                    {exit, terminated}
            end;
        false ->
            %% Echo needs to be set to true, otherwise the
            %% password will be printed to the shell and we
            %% do not want that.
            {error, {error, enotsup}, []}
    end.

get_chars_n(Prompt, M, F, Xa, Drv, Shell, Buf, Encoding) ->
    Pbs = prompt_bytes(Prompt, Encoding),
    case get(echo) of
        true ->
            get_chars_loop(Pbs, M, F, Xa, Drv, Shell, Buf, start, [], Encoding);
        false ->
            get_chars_n_loop(Pbs, M, F, Xa, Drv, Shell, Buf, start, Encoding)
    end.

get_chars_line(Prompt, M, F, Xa, Drv, Shell, Buf, Encoding) ->
    Pbs = prompt_bytes(Prompt, Encoding),
    get_chars_loop(Pbs, M, F, Xa, Drv, Shell, Buf, start, [], Encoding).

get_chars_loop(Pbs, M, F, Xa, Drv, Shell, Buf0, State, LineCont0, Encoding) ->
    Result = case not(get(dumb)) andalso get(echo) of
                 true ->
                     get_line(Buf0, Pbs, LineCont0, Drv, Shell, Encoding);
                 false ->
                     get_line_echo_off(Buf0, Encoding, Pbs, Drv, Shell)
             end,
    case Result of
        {done,LineCont1,Buf} ->
            get_chars_apply(Pbs, M, F, Xa, Drv, Shell, append(Buf, [], Encoding),
                            State, LineCont1, Encoding);
        {no_translation, unicode, latin1} ->
            {error,{error,{no_translation, unicode, latin1}}, []};
        interrupted ->
            {error,{error,interrupted},[]};
        terminated ->
            {exit,terminated}
    end.

get_chars_apply(Pbs, M, F, Xa, Drv, Shell, Buf, State0, LineCont, Encoding) ->
    %% multi line support means that we should not keep the state
    %% but we need to keep it for oldshell mode
    {State, Line} = case not(get(dumb)) andalso get(echo) of
                        true -> {start, edlin:current_line(LineCont)};
                        false -> {State0, LineCont}
                    end,
    case catch M:F(State, cast(Line,get(read_mode), Encoding), Encoding, Xa) of
        {stop,eof,_} ->
            {ok,eof,eof};
        {stop,Result,eof} ->
            {ok,Result,eof};
        {stop,Result,Rest} ->
            %% Prompt was valid expression, clear the prompt in user_drv
            %% First redraw without the multi line prompt
            FormattedLine = format_expression(LineCont, Drv),
            case LineCont of
                {[_|_], _, _} ->
                    [CL1|LB1] = lists:reverse(string:split(FormattedLine, "\n", all)),
                    LineCont1 = {LB1,{lists:reverse(CL1++"\n"), []},[]},
                    MultiLinePrompt = lists:duplicate(shell:prompt_width(Pbs), $\s),
                    send_drv_reqs(Drv, [{redraw_prompt, Pbs, MultiLinePrompt, LineCont1},new_prompt]);
                _ -> skip %% oldshell mode
            end,
            _ = case {M,F} of
                    {io_lib, get_until} ->
                        save_line_buffer(string:trim(FormattedLine, both)++"\n", get_lines(new_stack(get(line_buffer))));
                    _ ->
                        skip
                end,
            {ok,Result,append(Rest, Buf, Encoding)};
        {'EXIT',_} ->
            {error,{error,err_func(M, F, Xa)},[]};
        State1 ->
            get_chars_loop(Pbs, M, F, Xa, Drv, Shell, Buf, State1, LineCont, Encoding)
    end.

get_chars_n_loop(Pbs, M, F, Xa, Drv, Shell, Buf0, State, Encoding) ->
    case check_encoding(Buf0, Encoding) of
        false ->
            {error,{error,{no_translation,unicode,Encoding}},[]};
        true ->
            try M:F(State, cast(Buf0, get(read_mode), Encoding), Encoding, Xa) of
                {stop,eof,_} ->
                    {ok, eof, eof};
                {stop,Result,Rest} ->
                    {ok, Result, append(Rest,[],Encoding)};
                State1 ->
                    case get_chars_echo_off(Pbs, Drv, Shell) of
                        interrupted ->
                            {error,{error,interrupted},[]};
                        terminated ->
                            {exit,terminated};
                        Buf ->
                            get_chars_n_loop(Pbs, M, F, Xa, Drv, Shell, Buf, State1, Encoding)
                    end
            catch _:_ ->
                    {error,{error,err_func(M, F, Xa)},[]}
            end
    end.

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.

%% get_line(Chars, PromptBytes, Drv)
%%  Get a line with eventual line editing. Handle other io requests
%%  while getting line.
%%  Returns:
%%      {done,LineChars,RestChars}
%%      interrupted
get_line(Chars, Pbs, Cont, Drv, Shell, Encoding) ->
    {more_chars,Cont1,Rs} = case Cont of
                                [] -> edlin:start(Pbs);
                                _ -> edlin:start(Pbs, Cont)
                            end,
    send_drv_reqs(Drv, Rs),
    get_line1(edlin:edit_line(Chars, Cont1), Drv, Shell, new_stack(get(line_buffer)),
              Encoding).

get_line1({done, Cont, Rest, Rs}, Drv, _Shell, _Ls, _Encoding) ->
    send_drv_reqs(Drv, Rs),
    {done, Cont, Rest};
get_line1({open_editor, _Cs, Cont, Rs}, Drv, Shell, Ls0, Encoding) ->
    send_drv_reqs(Drv, Rs),
    Buffer = edlin:current_line(Cont),
    send_drv(Drv, {open_editor, Buffer}),
    receive
        {Drv, {editor_data, Cs1}} ->
            send_drv_reqs(Drv, edlin:erase_line()),
            {more_chars,NewCont,NewRs} = edlin:start(edlin:prompt(Cont)),
            send_drv_reqs(Drv, NewRs),
            get_line1(edlin:edit_line(Cs1, NewCont), Drv, Shell, Ls0, Encoding)
    end;
get_line1({format_expression, _Cs, {line, _, _, _} = Cont, Rs}, Drv, Shell, Ls, Encoding) ->
    send_drv_reqs(Drv, Rs),
    Cs1 = format_expression(Cont, Drv),
    send_drv_reqs(Drv, edlin:erase_line()),
    {more_chars,NewCont,NewRs} = edlin:start(edlin:prompt(Cont)),
    send_drv_reqs(Drv, NewRs),
    get_line1(edlin:edit_line(Cs1, NewCont), Drv, Shell, Ls, Encoding);
%% Move Up, Down in History: Ctrl+P, Ctrl+N
get_line1({history_up,Cs,{_,_,_,Mode0}=Cont,Rs}, Drv, Shell, Ls0, Encoding) ->
    send_drv_reqs(Drv, Rs),
    case up_stack(save_line(Ls0, edlin:current_line(Cont))) of
        {none,_Ls} ->
            send_drv(Drv, beep),
            get_line1(edlin:edit_line(Cs, Cont), Drv, Shell, Ls0, Encoding);
        {Lcs,Ls} ->
            send_drv_reqs(Drv, edlin:erase_line()),
            {more_chars,{A,B,C,_},Nrs} = edlin:start(edlin:prompt(Cont)),
            Ncont = {A,B,C,Mode0},
            send_drv_reqs(Drv, Nrs),
            get_line1(
              edlin:edit_line1(
                string:to_graphemes(
                  lists:sublist(Lcs, 1, length(Lcs)-1)),
                Ncont),
              Drv, Shell, Ls, Encoding)
    end;
get_line1({history_down,Cs,{_,_,_,Mode0}=Cont,Rs}, Drv, Shell, Ls0, Encoding) ->
    send_drv_reqs(Drv, Rs),
    case down_stack(save_line(Ls0, edlin:current_line(Cont))) of
        {none,_Ls} ->
            send_drv(Drv, beep),
            get_line1(edlin:edit_line(Cs, Cont), Drv, Shell, Ls0, Encoding);
        {Lcs,Ls} ->
            send_drv_reqs(Drv, edlin:erase_line()),
            {more_chars,{A,B,C,_},Nrs} = edlin:start(edlin:prompt(Cont)),
            Ncont = {A,B,C,Mode0},
            send_drv_reqs(Drv, Nrs),
            get_line1(edlin:edit_line1(string:to_graphemes(lists:sublist(Lcs,
                                                                         1,
                                                                         length(Lcs)-1)),
                                       Ncont),
                      Drv,
                      Shell,
                      Ls, Encoding)
    end;
%% ^R = backward search, ^S = forward search.
%% Search is tricky to implement and does a lot of back-and-forth
%% work with edlin.erl (from stdlib). Edlin takes care of writing
%% and handling lines and escape characters to get out of search,
%% whereas this module does the actual searching and appending to lines.
%% Erlang's shell wasn't exactly meant to traverse the wall between
%% line and line stack, so we at least restrict it by introducing
%% new modes: search, search_quit, search_found. These are added to
%% the regular ones (none, meta_left_sq_bracket) and handle special
%% cases of history search.
get_line1({search,Cs,Cont,Rs}, Drv, Shell, Ls, Encoding) ->
    send_drv_reqs(Drv, Rs),
    %% drop current line, move to search mode. We store the current
    %% prompt ('N>') and substitute it with the search prompt.
    put(search_quit_prompt, Cont),
    Pbs = prompt_bytes("\033[;1;4msearch:\033[0m ", Encoding),
    {more_chars,Ncont,_Nrs} = edlin:start(Pbs, {search,none}),
    put(search, new_search),
    get_line1(edlin:edit_line1(Cs, Ncont), Drv, Shell, Ls, Encoding);
get_line1({help, Before, Cs0, Cont, Rs}, Drv, Shell, Ls0, Encoding) ->
    send_drv_reqs(Drv, Rs),
    {_,Word,_} = edlin:over_word(Before, [], 0),
    {R,Docs} = case edlin_context:get_context(Before) of
        {function, Mod} when Word =/= [] -> try
                    {ok, [{atom,_,Module}], _} = erl_scan:string(Mod),
                    {ok, [{atom,_,Word1}], _} = erl_scan:string(Word),
                    {function, c:h1(Module, Word1)}
                catch _:_ ->
                    {ok, [{atom,_,Module1}], _} = erl_scan:string(Mod),
                    {module, c:h1(Module1)}
                end;
        {function, Mod} ->
            {ok, [{atom,_,Module}], _} = erl_scan:string(Mod),
            {module, c:h1(Module)};
        {function, Mod, Fun, _Args, _Unfinished, _Nesting} ->
            {ok, [{atom,_,Module}], _} = erl_scan:string(Mod),
            {ok, [{atom,_,Function}], _} = erl_scan:string(Fun),
            {function, c:h1(Module, Function)};
        {term, _, {atom, Word1}}->
            {ok, [{atom,_,Module}], _} = erl_scan:string(Word1),
            {module, c:h1(Module)};
        _ -> {error, {error, no_help}}
    end,
    case {R, Docs} of
        {_, {error, _}} -> send_drv(Drv, beep);
        {module, _} ->
                Docs1 = "  "++string:trim(lists:nthtail(3, Docs),both),
                send_drv(Drv, {put_expand, unicode,
                    [unicode:characters_to_binary(Docs1)], 7});
        {function, _} ->
                Docs1 = "  "++string:trim(Docs,both),
                send_drv(Drv, {put_expand, unicode,
                    [unicode:characters_to_binary(Docs1)], 7})
    end,
    get_line1(edlin:edit_line(Cs0, Cont), Drv, Shell, Ls0, Encoding);
get_line1({Expand, Before, Cs0, Cont,Rs}, Drv, Shell, Ls0, Encoding)
  when Expand =:= expand; Expand =:= expand_full ->
    send_drv_reqs(Drv, Rs),
    ExpandFun = get(expand_fun),
    {Found, CompleteChars, Matches} = ExpandFun(Before, []),
    case Found of
        no -> send_drv(Drv, beep);
        _ -> ok
    end,
    {Width, _Height} = get_tty_geometry(Drv),
    Cs1 = append(CompleteChars, Cs0, Encoding),

    MatchStr = case Matches of
                   [] -> [];
                   _ -> edlin_expand:format_matches(Matches, Width)
               end,
    Cs = case {Cs1, MatchStr} of
             {_, []} -> Cs1;
             {Cs1, _} when Cs1 =/= [] -> Cs1;
             _ ->
                 NlMatchStr = unicode:characters_to_binary("\n"++MatchStr),
                 NLines = case Expand of
                                expand -> 7;
                                expand_full -> 0
                            end,
                 case get(expand_below) of
                     true ->
                        send_drv(Drv, {put_expand, unicode, unicode:characters_to_binary(string:trim(MatchStr, trailing)), NLines}),
                        Cs1;
                     false ->
                         send_drv(Drv, {put_chars, unicode, NlMatchStr}),
                         [$\e, $l | Cs1]
                 end
         end,
    get_line1(edlin:edit_line(Cs, Cont), Drv, Shell, Ls0, Encoding);

%% The search item was found and accepted (new line entered on the exact
%% result found)
get_line1({search_found,_Cs,_,Rs}, Drv, Shell, Ls0, Encoding) ->
    SearchResult = get(search_result),
    LineCont = case SearchResult of
                   [] -> {[],{[],[]},[]};
                   _ -> [Last| LB] = lists:reverse(SearchResult),
                        {LB, {lists:reverse(Last),[]},[]}
               end,
    Prompt = edlin:prompt(get(search_quit_prompt)),
    send_drv_reqs(Drv, Rs),
    send_drv_reqs(Drv, edlin:erase_line()),
    send_drv_reqs(Drv, edlin:redraw_line({line, Prompt, LineCont, {normal,none}})),
    put(search_result, []),
    get_line1({done, LineCont, "\n", Rs}, Drv, Shell, Ls0, Encoding);
%% The search mode has been exited, but the user wants to remain in line
%% editing mode wherever that was, but editing the search result.
get_line1({search_quit,_Cs,_,Rs}, Drv, Shell, Ls, Encoding) ->
    %% Load back the old prompt with the correct line number.
    case edlin:prompt(get(search_quit_prompt)) of
        Prompt -> % redraw the line and keep going with the same stack position
            SearchResult = get(search_result),
            L = case SearchResult of
                    [] -> {[],{[],[]},[]};
                    _  -> [Last|LB] = lists:reverse(SearchResult),
                          {LB, {lists:reverse(Last), []}, []}
                end,
            NCont = {line,Prompt,L,{normal,none}},
            put(search_result, []),
            send_drv_reqs(Drv, [delete_line|Rs]),
            send_drv_reqs(Drv, edlin:redraw_line(NCont)),
            get_line1({more_chars, NCont ,[]}, Drv, Shell, pad_stack(Ls), Encoding)
    end;
get_line1({search_cancel,_Cs,_,Rs}, Drv, Shell, Ls, Encoding) ->
    NCont = get(search_quit_prompt),
    put(search_result, []),
    send_drv_reqs(Drv, [delete_line|Rs]),
    send_drv_reqs(Drv, edlin:redraw_line(NCont)),
    get_line1({more_chars, NCont, []}, Drv, Shell, Ls, Encoding);
%% Search mode is entered.
get_line1({What,{line,Prompt,{_,{RevCmd0,_},_},{search, none}}=Cont0,Rs},
          Drv, Shell, Ls0, Encoding) ->
    %% Figure out search direction. ^S and ^R are returned through edlin
    %% whenever we received a search while being already in search mode.
    OldSearch = get(search),
    {Search, Ls1, RevCmd} = case RevCmd0 of
                                [$\^S|RevCmd1] ->
                                    {fun search_down_stack/2, Ls0, RevCmd1};
                                [$\^R|RevCmd1] ->
                                    {fun search_up_stack/2, Ls0, RevCmd1};
                                _ when RevCmd0 =/= OldSearch -> % new search, rewind stack for a proper search.
                                    {fun search_up_stack/2, new_stack(get_lines(Ls0)), RevCmd0};
                                _ ->
                                    {skip, Ls0, RevCmd0}
                            end,
    put(search, RevCmd),
    Cmd = lists:reverse(RevCmd),
    if Search =:= skip ->
        %% Move expand are the only valid requests to bypass search mode
        %% Sending delete_chars, insert_chars, etc. will result in
        %% expand area being cleared.
        Rs1 = [R||{move_expand,_}=R<-Rs],
        send_drv_reqs(Drv, Rs1),
        more_data(What, Cont0, Drv, Shell, Ls0, Encoding);
       true ->
        {Ls, NewStack} = case Search(Ls1, Cmd) of
            {none, Ls2} ->
                send_drv(Drv, beep),
                put(search_result, []),
                send_drv(Drv, delete_line),
                send_drv(Drv, {insert_chars, unicode, unicode:characters_to_binary(Prompt++Cmd)}),
                {Ls2, {[],{RevCmd, []},[]}};
            {Line, Ls2} -> % found. Complete the output edlin couldn't have done.
                Lines = string:split(string:to_graphemes(Line), "\n", all),
                put(search_result, Lines),
                send_drv(Drv, delete_line),
                send_drv(Drv, {insert_chars, unicode, unicode:characters_to_binary(Prompt++Cmd)}),
                send_drv(Drv, {put_expand, unicode, unicode:characters_to_binary("  "++lists:join("\n  ",Lines)), 7}),
                {Ls2, {[],{RevCmd, []},[]}}
        end,
        Cont = {line,Prompt,NewStack,{search, none}},
        more_data(What, Cont, Drv, Shell, Ls, Encoding)
    end;
get_line1({What,Cont0,Rs}, Drv, Shell, Ls, Encoding) ->
    send_drv_reqs(Drv, Rs),
    more_data(What, Cont0, Drv, Shell, Ls, Encoding).

more_data(What, Cont0, Drv, Shell, Ls, Encoding) ->
    receive
        {Drv, activate} ->
            send_drv_reqs(Drv, edlin:redraw_line(Cont0)),
            more_data(What, Cont0, Drv, Shell, Ls, Encoding);
        {Drv,{data,Cs}} ->
            Res = edlin:edit_line(cast(Cs, list), Cont0),
            get_line1(Res,
                      Drv, Shell, Ls, Encoding);
        {Drv,eof} ->
            get_line1(edlin:edit_line(eof, Cont0), Drv, Shell, Ls, Encoding);
        {io_request,From,ReplyAs,Req} when is_pid(From) ->
            {more_chars,Cont,_More} = edlin:edit_line([], Cont0),
            send_drv_reqs(Drv, edlin:erase_line()),
            io_request(Req, From, ReplyAs, Drv, Shell, []), %WRONG!!!
            send_drv_reqs(Drv, edlin:redraw_line(Cont)),
            get_line1({more_chars,Cont,[]}, Drv, Shell, Ls, Encoding);
        {reply,{From,ReplyAs},Reply} ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
            more_data(What, Cont0, Drv, Shell, Ls, Encoding);
        {'EXIT',Drv,interrupt} ->
            interrupted;
        {'EXIT',Drv,_} ->
            terminated;
        {'EXIT',Shell,R} ->
            exit(R)
    after
        get_line_timeout(What)->
            get_line1(edlin:edit_line([], Cont0), Drv, Shell, Ls, Encoding)
    end.

get_line_echo_off(Chars, ToEnc, Pbs, Drv, Shell) ->
    send_drv_reqs(Drv, [{put_chars, unicode,Pbs}]),
    case get_line_echo_off1(edit_line(Chars,[]), Drv, Shell) of
        {done, Line, _Rest} = Res when ToEnc =:= latin1 ->
            case check_encoding(Line, ToEnc) of
                false ->
                    {no_translation, unicode, ToEnc};
                true ->
                    Res
            end;
        Res ->
            Res
    end.

get_line_echo_off1({Chars,[],Rs}, Drv, Shell) ->
    case get(echo) of
        true -> send_drv_reqs(Drv, Rs);
        false -> skip
    end,
    receive
	{Drv,{data,Cs}} ->
	    get_line_echo_off1(edit_line(cast(Cs, list), Chars), Drv, Shell);
	{Drv,eof} ->
	    get_line_echo_off1(edit_line(eof, Chars), Drv, Shell);
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
	    io_request(Req, From, ReplyAs, Drv, Shell, []),
	    get_line_echo_off1({Chars,[],[]}, Drv, Shell);
        {reply,{From,ReplyAs},Reply} when From =/= undefined ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
            get_line_echo_off1({Chars,[],[]},Drv, Shell);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated;
	{'EXIT',Shell,R} ->
	    exit(R)
    end;
get_line_echo_off1(eof, _Drv, _Shell) ->
    {done,eof,eof};
get_line_echo_off1({Chars,Rest,Rs}, Drv, _Shell) ->
    case get(echo) of
        true -> send_drv_reqs(Drv, Rs);
        false -> skip
    end,
    {done,lists:reverse(Chars),case Rest of done -> []; _ -> Rest end}.
get_chars_echo_off(Pbs, Drv, Shell) ->
    send_drv_reqs(Drv, [{insert_chars, unicode,Pbs}]),
    get_chars_echo_off1(Drv, Shell).

get_chars_echo_off1(Drv, Shell) ->
    receive
        {Drv, {data, Cs}} ->
            cast(Cs, list);
        {Drv, eof} ->
            eof;
        {io_request,From,ReplyAs,Req} when is_pid(From) ->
            io_request(Req, From, ReplyAs, Drv, Shell, []),
            get_chars_echo_off1(Drv, Shell);
        {reply,{From,ReplyAs},Reply} when From =/= undefined ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
            get_chars_echo_off1(Drv, Shell);
        {'EXIT',Drv,interrupt} ->
            interrupted;
        {'EXIT',Drv,_} ->
            terminated;
        {'EXIT',Shell,R} ->
            exit(R)
    end.

format_expression(Cont, Drv) ->
    FormatingCommand = application:get_env(stdlib, format_shell_func, default),
    Buffer = edlin:current_line(Cont),
    try
        case FormatingCommand of
            default ->
                string:trim(Buffer, trailing, "\n");
            {M,F} when is_atom(M), is_atom(F) ->
                M:F(Buffer);
            FormatingCommand1 when is_list(FormatingCommand1) ->
                format_expression1(Buffer, FormatingCommand1)
        end
    catch _:_ ->
            send_drv_reqs(Drv, [{put_chars, unicode, io_lib:format("* Bad format function: ~tp~n", [FormatingCommand])}]),
            _ = shell:format_shell_func(default),
            string:trim(Buffer, trailing, "\n")
    end.
format_expression1(Buffer, FormatingCommand) ->
    %% Write the current expression to a file, format it with a formatting tool
    %% provided by the user and read the file back
    MkTemp = case os:type() of
        {win32, _} ->
            os:cmd("powershell \"write-host (& New-TemporaryFile | Select-Object -ExpandProperty FullName)\"");
        {unix,_} ->
            os:cmd("mktemp")
    end,
    TmpFile = string:chomp(MkTemp) ++ ".erl",
    _ = file:write_file(TmpFile, unicode:characters_to_binary(Buffer, unicode)),
    FormattingCommand1 = string:replace(FormatingCommand, "${file}", TmpFile),
    _ = os:cmd(FormattingCommand1),
    {ok, Content} = file:read_file(TmpFile),
    _ = file:del_dir_r(TmpFile),
    Unicode = case unicode:characters_to_list(Content,unicode) of
                  {error, _, _} -> unicode:characters_to_list(
                                     unicode:characters_to_list(Content,latin1), unicode);
                  U -> U
              end,
    string:chomp(Unicode).

%% Edit line is used in echo=false mode which has two users
%% Either we are running in "oldshell" or we run using "noshell".
%%
%% For "oldshell" we need to take care of certain special characters
%% that can be entered, but for "noshell" we don't want to do any of
%% that.
edit_line(Input, State) ->
    case get(noshell) of
        false ->
            edit_line(Input, State, []);
        true ->
            edit_line_raw(Input, State, [])
    end.

%% We support line editing for the ICANON mode except the following
%% line editing characters, which already has another meaning in
%% echo-on mode (See Advanced Programming in the Unix Environment, 2nd ed,
%% Stevens, page 638):
%% - ^u in posix/icanon mode: erase-line, prefix-arg in edlin
%% - ^t in posix/icanon mode: status, transpose-char in edlin
%% - ^d in posix/icanon mode: eof, delete-forward in edlin
%% - ^r in posix/icanon mode: reprint (silly in echo-off mode :-))
%% - ^w in posix/icanon mode: word-erase (produces a beep in edlin)
edit_line(eof, [], _) ->
    eof;
edit_line(eof, Chars, Rs) ->
    {Chars,eof, lists:reverse(Rs)};
edit_line([],Chars, Rs) ->
    {Chars,[],lists:reverse(Rs)};
edit_line([$\r,$\n|Cs],Chars, Rs) ->
    {[$\n | Chars], remainder_after_nl(Cs), lists:reverse([{put_chars, unicode, "\n"}|Rs])};
edit_line([NL|Cs],Chars, Rs) when NL =:= $\r; NL =:= $\n ->
    {[$\n | Chars], remainder_after_nl(Cs), lists:reverse([{put_chars, unicode, "\n"}|Rs])};
edit_line([Erase|Cs],[], Rs) when Erase =:= $\177; Erase =:= $\^H ->
    edit_line(Cs,[], Rs);
edit_line([Erase|Cs],[_|Chars], Rs) when Erase =:= $\177; Erase =:= $\^H ->
    edit_line(Cs,Chars, [{delete_chars, -1}|Rs]);
edit_line([CtrlChar|Cs],Chars, Rs) when CtrlChar < 32 ->
    edit_line(Cs,Chars,Rs);
edit_line([Char|Cs],Chars, Rs) ->
    edit_line(Cs,[Char|Chars], [{put_chars, unicode, [Char]}|Rs]).

edit_line_raw(eof, [], _) ->
    eof;
edit_line_raw(eof, Chars, Rs) ->
    {Chars,eof, lists:reverse(Rs)};
edit_line_raw([],Chars, Rs) ->
    {Chars,[],lists:reverse(Rs)};
edit_line_raw([NL|Cs],Chars, Rs) when NL =:= $\n ->
    {[$\n | Chars], remainder_after_nl(Cs), lists:reverse([{put_chars, unicode, "\n"}|Rs])};
edit_line_raw([Char|Cs],Chars, Rs) ->
    edit_line_raw(Cs,[Char|Chars], [{put_chars, unicode, [Char]}|Rs]).

remainder_after_nl("") -> done;
remainder_after_nl(Cs) -> Cs.

get_line_timeout(blink) -> 1000;
get_line_timeout(more_chars) -> infinity.

new_stack(Ls) -> {stack,Ls,{},[]}.

up_stack({stack,[L|U],{},D}) ->
    {L,{stack,U,L,D}};
up_stack({stack,[],{},D}) ->
    {none,{stack,[],{},D}};
up_stack({stack,U,C,D}) ->
    up_stack({stack,U,{},[C|D]}).

down_stack({stack,U,{},[L|D]}) ->
    {L,{stack,U,L,D}};
down_stack({stack,U,{},[]}) ->
    {none,{stack,U,{},[]}};
down_stack({stack,U,C,D}) ->
    down_stack({stack,[C|U],{},D}).

save_line({stack, U, {}, []}, Line) ->
    {stack, U, {}, [Line]};
save_line({stack, U, _L, D}, Line) ->
    {stack, U, Line, D}.

get_lines(Ls) -> get_all_lines(Ls).

%% There's a funny behaviour whenever the line stack doesn't have a "\n"
%% at its end -- get_lines() seemed to work on the assumption it *will* be
%% there, but the manipulations done with search history do not require it.
%%
%% It is an assumption because the function was built with either the full
%% stack being on the 'Up' side (we're on the new line) where it isn't
%% stripped. The only other case when it isn't on the 'Up' side is when
%% someone has used the up/down arrows (or ^P and ^N) to navigate lines,
%% in which case, a line with only a \n is stored at the end of the stack
%% (the \n is returned by edlin:current_line/1).
%%
%% get_all_lines works the same as get_lines, but only strips the trailing
%% character if it's a linebreak. Otherwise it's kept the same. This is
%% because traversing the stack due to search history will *not* insert
%% said empty line in the stack at the same time as other commands do,
%% and thus it should not always be stripped unless we know a new line
%% is the last entry.
get_all_lines({stack, U, {}, []}) ->
    U;
get_all_lines({stack, U, {}, D}) ->
    case lists:reverse(D, U) of
        ["\n"|Lines] -> Lines;
        Lines -> Lines
    end;
get_all_lines({stack, U, L, D}) ->
    get_all_lines({stack, U, {}, [L|D]}).

%% For the same reason as above, though, we need to expand the stack
%% in some cases to make sure we play nice with up/down arrows. We need
%% to insert newlines, but not always.
pad_stack({stack, U, L, D}) ->
    {stack, U, L, D++["\n"]}.

save_line_buffer("\n", Lines) ->
    save_line_buffer(Lines);
save_line_buffer(Line, [Line|_Lines]=Lines) ->
    save_line_buffer(Lines);
save_line_buffer(Line, Lines) ->
    try
        group_history:add(Line)
    catch E:R:ST ->
            ?LOG_ERROR(#{ msg => "Failed to write to shell history",
                          error => {E, R, ST} })
    end,
    save_line_buffer([Line|Lines]).

save_line_buffer(Lines) ->
    put(line_buffer, Lines).

search_up_stack(Stack, Substr) ->
    case up_stack(Stack) of
        {none,NewStack} -> {none,NewStack};
        {L, NewStack} ->
            case string:find(L, Substr) of
                nomatch -> search_up_stack(NewStack, Substr);
                _ -> {string:trim(L, trailing, "$\n"), NewStack}
            end
    end.

search_down_stack(Stack, Substr) ->
    case down_stack(Stack) of
        {none,NewStack} -> {none,NewStack};
        {L, NewStack} ->
            case string:find(L, Substr) of
                nomatch -> search_down_stack(NewStack, Substr);
                _ -> {string:trim(L, trailing, "$\n"), NewStack}
            end
    end.


%% This is get_line without line editing (except for backspace) and
%% without echo.
get_password_line(Chars, Drv, Shell) ->
    get_password1(edit_password(Chars,[]),Drv,Shell).

get_password1({Chars,[]}, Drv, Shell) ->
    receive
	{Drv,{data,Cs}} ->
	    get_password1(edit_password(cast(Cs,list),Chars),Drv,Shell);
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
	    io_request(Req, From, ReplyAs, Drv, Shell, []), %WRONG!!!
	    %% I guess the reason the above line is wrong is that Buf is
	    %% set to []. But do we expect anything but plain output?

            get_password1({Chars, []}, Drv, Shell);
        {reply,{From,ReplyAs},Reply} ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
	    get_password1({Chars, []}, Drv, Shell);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated;
	{'EXIT',Shell,R} ->
	    exit(R)
    end;
get_password1({Chars,Rest},Drv,_Shell) ->
    send_drv_reqs(Drv,[{insert_chars, unicode, "\n"}]),
    {done,lists:reverse(Chars),case Rest of done -> []; _ -> Rest end}.

edit_password([],Chars) ->
    {Chars,[]};
edit_password([$\r],Chars) ->
    {Chars,done};
edit_password([$\r|Cs],Chars) ->
    {Chars,Cs};
edit_password([$\177|Cs],[]) ->       %% Being able to erase characters is
    edit_password(Cs,[]);             %% the least we should offer, but
edit_password([$\177|Cs],[_|Chars]) ->%% is backspace enough?
    edit_password(Cs,Chars);
edit_password([Char|Cs],Chars) ->
    edit_password(Cs,[Char|Chars]).

%% prompt_bytes(Prompt, Encoding)
%%  Return a flat list of characters for the Prompt.
prompt_bytes(Prompt, Encoding) ->
    lists:flatten(io_lib:format_prompt(Prompt, Encoding)).

cast(Buf, Type) ->
    cast(Buf, Type, utf8).
cast(eof, _, _) ->
    eof;
cast(L, binary, ToEnc) ->
    unicode:characters_to_binary(L, utf8, ToEnc);
cast(L, list, _ToEnc) when is_list(L) ->
    L;
cast(L, list, _ToEnc) ->
    unicode:characters_to_list(L, utf8).

append(eof, [], _) ->
    eof;
append(eof, L, _) ->
    L;
append(L, [], _) when is_list(L) ->
    %% When doing ++ all of L needs to be traversed to check if it is
    %% a proper list. Since we know it is a proper list we just return
    %% the list without checking.
    L;
append(L, A, _) when is_list(L) ->
    L ++ A; %% We know L is valid unicode, so we just append the two
append(B, L, FromEnc) ->
    append(unicode:characters_to_list(B, FromEnc), L, FromEnc).

check_encoding(eof, _) ->
    true;
check_encoding(ListOrBinary, unicode) when is_list(ListOrBinary); is_binary(ListOrBinary) ->
    true;
check_encoding(List, latin1) when is_list(List) ->
    is_latin1(List).

is_latin1([H|T]) when 0 =< H, H =< 255 ->
    is_latin1(T);
is_latin1([]) ->
    true;
is_latin1(_) ->
    false.
