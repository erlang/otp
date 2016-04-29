%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

%% A group leader process for user io.

-export([start/2, start/3, server/3]).
-export([interfaces/1]).

start(Drv, Shell) ->
    start(Drv, Shell, []).

start(Drv, Shell, Options) ->
    spawn_link(group, server, [Drv, Shell, Options]).

server(Drv, Shell, Options) ->
    process_flag(trap_exit, true),
    edlin:init(),
    put(line_buffer, proplists:get_value(line_buffer, Options, [])),
    put(read_mode, list),
    put(user_drv, Drv),
    put(expand_fun,
	proplists:get_value(expand_fun, Options,
			    fun(B) -> edlin_expand:expand(B) end)),
    put(echo, proplists:get_value(echo, Options, true)),
    
    start_shell(Shell),
    server_loop(Drv, get(shell), []).

%% Return the pid of user_drv and the shell process.
%% Note: We can't ask the group process for this info since it
%% may be busy waiting for data from the driver.
interfaces(Group) ->
    case process_info(Group, dictionary) of
	{dictionary,Dict} ->
	    get_pids(Dict, [], false);
	_ ->
	    []
    end.

get_pids([Drv = {user_drv,_} | Rest], Found, _) ->
    get_pids(Rest, [Drv | Found], true);
get_pids([Sh = {shell,_} | Rest], Found, Active) ->
    get_pids(Rest, [Sh | Found], Active);
get_pids([_ | Rest], Found, Active) ->
    get_pids(Rest, Found, Active);
get_pids([], Found, true) ->
    Found;
get_pids([], _Found, false) ->
    [].

%% start_shell(Shell)
%%  Spawn a shell with its group_leader from the beginning set to ourselves.
%%  If Shell a pid the set its group_leader.

start_shell({Mod,Func,Args}) ->
    start_shell1(Mod, Func, Args);
start_shell({Node,Mod,Func,Args}) ->
    start_shell1(net, call, [Node,Mod,Func,Args]);
start_shell(Shell) when is_atom(Shell) ->
    start_shell1(Shell, start, []);
start_shell(Shell) when is_function(Shell) ->
    start_shell1(Shell);
start_shell(Shell) when is_pid(Shell) ->
    group_leader(self(), Shell),		% we are the shells group leader
    link(Shell),				% we're linked to it.
    put(shell, Shell);
start_shell(_Shell) ->
    ok.

start_shell1(M, F, Args) ->
    G = group_leader(),
    group_leader(self(), self()),
    case catch apply(M, F, Args) of
	Shell when is_pid(Shell) ->
	    group_leader(G, self()),
	    link(Shell),			% we're linked to it.
	    put(shell, Shell);
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
	    put(shell, Shell);
	Error ->				% start failure
	    exit(Error)				% let the group process crash
    end.

server_loop(Drv, Shell, Buf0) ->
    receive
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
            %% This io_request may cause a transition to a couple of
            %% selective receive loops elsewhere in this module.
            Buf = io_request(Req, From, ReplyAs, Drv, Buf0),
            server_loop(Drv, Shell, Buf);
        {reply,{{From,ReplyAs},Reply}} ->
            io_reply(From, ReplyAs, Reply),
	    server_loop(Drv, Shell, Buf0);
	{driver_id,ReplyTo} ->
	    ReplyTo ! {self(),driver_id,Drv},
	    server_loop(Drv, Shell, Buf0);
	{Drv, echo, Bool} ->
	    put(echo, Bool),
	    server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,interrupt} ->
	    %% Send interrupt to the shell.
	    exit_shell(interrupt),
	    server_loop(Drv, Shell, Buf0);
	{'EXIT',Drv,R} ->
	    exit(R);
	{'EXIT',Shell,R} ->
	    exit(R);
	%% We want to throw away any term that we don't handle (standard
	%% practice in receive loops), but not any {Drv,_} tuples which are
	%% handled in io_request/5.
	NotDrvTuple when (not is_tuple(NotDrvTuple)) orelse
			 (tuple_size(NotDrvTuple) =/= 2) orelse
			 (element(1, NotDrvTuple) =/= Drv) ->
	    %% Ignore this unknown message.
	    server_loop(Drv, Shell, Buf0)
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
			   

io_request(Req, From, ReplyAs, Drv, Buf0) ->
    case io_request(Req, Drv, {From,ReplyAs}, Buf0) of
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
%%standard unicode
%% format.
%% You might be tempted to send binaries unchecked, but the driver 
%% expects unicode, so that is what we should send...
%% io_request({put_chars,unicode,Binary}, Drv, Buf) when is_binary(Binary) -> 
%%     send_drv(Drv, {put_chars,Binary}),
%%     {ok,ok,Buf};
%%
%% These put requests have to be synchronous to the driver as otherwise
%% there is no guarantee that the data has actually been printed.
io_request({put_chars,unicode,Chars}, Drv, From, Buf) ->
    case catch unicode:characters_to_binary(Chars,utf8) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode, Binary, {From,ok}}),
	    {noreply,Buf};
	_ ->
	    {error,{error,{put_chars, unicode,Chars}},Buf}
    end;
io_request({put_chars,unicode,M,F,As}, Drv, From, Buf) ->
    case catch apply(M, F, As) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode, Binary, {From,ok}}),
	    {noreply,Buf};
	Chars ->
	    case catch unicode:characters_to_binary(Chars,utf8) of
		B when is_binary(B) ->
		    send_drv(Drv, {put_chars_sync, unicode, B, {From,ok}}),
		    {noreply,Buf};
		_ ->
		    {error,{error,F},Buf}
	    end
    end;
io_request({put_chars,latin1,Binary}, Drv, From, Buf) when is_binary(Binary) -> 
    send_drv(Drv, {put_chars_sync, unicode,
                   unicode:characters_to_binary(Binary,latin1),
                   {From,ok}}),
    {noreply,Buf};
io_request({put_chars,latin1,Chars}, Drv, From, Buf) ->
    case catch unicode:characters_to_binary(Chars,latin1) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode, Binary, {From,ok}}),
	    {noreply,Buf};
	_ ->
	    {error,{error,{put_chars,latin1,Chars}},Buf}
    end;
io_request({put_chars,latin1,M,F,As}, Drv, From, Buf) ->
    case catch apply(M, F, As) of
	Binary when is_binary(Binary) ->
	    send_drv(Drv, {put_chars_sync, unicode,
                           unicode:characters_to_binary(Binary,latin1),
                           {From,ok}}),
	    {noreply,Buf};
	Chars ->
	    case catch unicode:characters_to_binary(Chars,latin1) of
		B when is_binary(B) ->
		    send_drv(Drv, {put_chars_sync, unicode, B, {From,ok}}),
		    {noreply,Buf};
		_ ->
		    {error,{error,F},Buf}
	    end
    end;

io_request({get_chars,Encoding,Prompt,N}, Drv, _From, Buf) ->
    get_chars(Prompt, io_lib, collect_chars, N, Drv, Buf, Encoding);
io_request({get_line,Encoding,Prompt}, Drv, _From, Buf) ->
    get_chars(Prompt, io_lib, collect_line, [], Drv, Buf, Encoding);
io_request({get_until,Encoding, Prompt,M,F,As}, Drv, _From, Buf) ->
    get_chars(Prompt, io_lib, get_until, {M,F,As}, Drv, Buf, Encoding);
io_request({get_password,_Encoding},Drv,_From,Buf) ->
    get_password_chars(Drv, Buf);
io_request({setopts,Opts}, Drv, _From, Buf) when is_list(Opts) ->
    setopts(Opts, Drv, Buf);
io_request(getopts, Drv, _From, Buf) ->
    getopts(Drv, Buf);
io_request({requests,Reqs}, Drv, From, Buf) ->
    io_requests(Reqs, {ok,ok,Buf}, From, Drv);

%% New in R12
io_request({get_geometry,columns},Drv,_From,Buf) ->
    case get_tty_geometry(Drv) of
	{W,_H} ->
	    {ok,W,Buf};
	_ ->
	    {error,{error,enotsup},Buf}
    end;
io_request({get_geometry,rows},Drv,_From,Buf) ->
    case get_tty_geometry(Drv) of
	{_W,H} ->
	    {ok,H,Buf};
	_ ->
	    {error,{error,enotsup},Buf}
    end;

%% BC with pre-R13
io_request({put_chars,Chars}, Drv, From, Buf) ->
    io_request({put_chars,latin1,Chars}, Drv, From, Buf);
io_request({put_chars,M,F,As}, Drv, From, Buf) ->
    io_request({put_chars,latin1,M,F,As}, Drv, From, Buf);
io_request({get_chars,Prompt,N}, Drv, From, Buf) ->
    io_request({get_chars,latin1,Prompt,N}, Drv, From, Buf);
io_request({get_line,Prompt}, Drv, From, Buf) ->
    io_request({get_line,latin1,Prompt}, Drv, From, Buf);
io_request({get_until, Prompt,M,F,As}, Drv, From, Buf) ->
    io_request({get_until,latin1, Prompt,M,F,As}, Drv, From, Buf);
io_request(get_password,Drv,From,Buf) ->
    io_request({get_password,latin1},Drv,From,Buf);



io_request(_, _Drv, _From, Buf) ->
    {error,{error,request},Buf}.

%% Status = io_requests(RequestList, PrevStat, From, Drv)
%%  Process a list of output requests as long as
%%  the previous status is 'ok' or noreply.
%%
%%  We use undefined as the From for all but the last request
%%  in order to discards acknowledgements from those requests.
%%
io_requests([R|Rs], {noreply,Buf}, From, Drv) ->
    ReqFrom = if Rs =:= [] -> From; true -> undefined end,
    io_requests(Rs, io_request(R, Drv, ReqFrom, Buf), From, Drv);
io_requests([R|Rs], {ok,ok,Buf}, From, Drv) ->
    ReqFrom = if Rs =:= [] -> From; true -> undefined end,
    io_requests(Rs, io_request(R, Drv, ReqFrom, Buf), From, Drv);
io_requests([_|_], Error, _From, _Drv) ->
    Error;
io_requests([], Stat, _From, _) ->
    Stat.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(undefined, _ReplyAs, _Reply) ->
    %% Ignore these replies as they are generated from io_requests/4.
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
check_valid_opts([{binary,_}|T]) ->
    check_valid_opts(T);
check_valid_opts([{encoding,Valid}|T]) when Valid =:= unicode; Valid =:= utf8; Valid =:= latin1 ->
    check_valid_opts(T);
check_valid_opts([{echo,_}|T]) ->
    check_valid_opts(T);
check_valid_opts([{expand_fun,_}|T]) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

do_setopts(Opts, Drv, Buf) ->
    put(expand_fun, proplists:get_value(expand_fun, Opts, get(expand_fun))),
    put(echo, proplists:get_value(echo, Opts, get(echo))),
    case proplists:get_value(encoding,Opts) of
	Valid when Valid =:= unicode; Valid =:= utf8 ->
	    set_unicode_state(Drv,true);
	latin1 ->
	    set_unicode_state(Drv,false);
	_ ->
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
	    {ok,ok,Buf};
	_ ->
	    {ok,ok,Buf}
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
    {ok,[Exp,Echo,Bin,Uni],Buf}.
    

%% get_chars(Prompt, Module, Function, XtraArgument, Drv, Buffer)
%%  Gets characters from the input Drv until as the applied function
%%  returns {stop,Result,Rest}. Does not block output until input has been
%%  received.
%%  Returns:
%%	{Result,NewSaveBuffer}
%%	{error,What,NewSaveBuffer}

get_password_chars(Drv,Buf) ->
    case get_password_line(Buf, Drv) of
	{done, Line, Buf1} ->
	    {ok, Line, Buf1};
	interrupted ->
	    {error, {error, interrupted}, []};
	terminated ->
	    {exit, terminated}
    end.

get_chars(Prompt, M, F, Xa, Drv, Buf, Encoding) ->
    Pbs = prompt_bytes(Prompt, Encoding),
    get_chars_loop(Pbs, M, F, Xa, Drv, Buf, start, Encoding).

get_chars_loop(Pbs, M, F, Xa, Drv, Buf0, State, Encoding) ->
    Result = case get(echo) of 
		 true ->
		     get_line(Buf0, Pbs, Drv, Encoding);
		 false ->
		     % get_line_echo_off only deals with lists
		     % and does not need encoding...
		     get_line_echo_off(Buf0, Pbs, Drv)
	     end,
    case Result of
	{done,Line,Buf1} ->
	    get_chars_apply(Pbs, M, F, Xa, Drv, Buf1, State, Line, Encoding);
	interrupted ->
	    {error,{error,interrupted},[]};
	terminated ->
	    {exit,terminated}
    end.

get_chars_apply(Pbs, M, F, Xa, Drv, Buf, State0, Line, Encoding) ->
    case catch M:F(State0, cast(Line,get(read_mode), Encoding), Encoding, Xa) of
	{stop,Result,Rest} ->
	    {ok,Result,append(Rest, Buf, Encoding)};
	{'EXIT',_} ->
	    {error,{error,err_func(M, F, Xa)},[]};
	State1 ->
	    get_chars_loop(Pbs, M, F, Xa, Drv, Buf, State1, Encoding)
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
%%	{done,LineChars,RestChars}
%%	interrupted

get_line(Chars, Pbs, Drv, Encoding) ->
    {more_chars,Cont,Rs} = edlin:start(Pbs),
    send_drv_reqs(Drv, Rs),
    get_line1(edlin:edit_line(Chars, Cont), Drv, new_stack(get(line_buffer)), 
	      Encoding).

get_line1({done,Line,Rest,Rs}, Drv, Ls, _Encoding) ->
    send_drv_reqs(Drv, Rs),
    save_line_buffer(Line, get_lines(Ls)),
    {done,Line,Rest};
get_line1({undefined,{_A,Mode,Char},Cs,Cont,Rs}, Drv, Ls0, Encoding) 
  when ((Mode =:= none) and (Char =:= $\^P))
       or ((Mode =:= meta_left_sq_bracket) and (Char =:= $A)) ->
    send_drv_reqs(Drv, Rs),
    case up_stack(save_line(Ls0, edlin:current_line(Cont))) of
	{none,_Ls} ->
	    send_drv(Drv, beep),
	    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls0, Encoding);
	{Lcs,Ls} ->
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    {more_chars,Ncont,Nrs} = edlin:start(edlin:prompt(Cont)),
	    send_drv_reqs(Drv, Nrs),
	    get_line1(edlin:edit_line1(lists:sublist(Lcs, 1, length(Lcs)-1),
				      Ncont),
		      Drv,
		      Ls, Encoding)
    end;
get_line1({undefined,{_A,Mode,Char},Cs,Cont,Rs}, Drv, Ls0, Encoding)
  when ((Mode =:= none) and (Char =:= $\^N))
       or ((Mode =:= meta_left_sq_bracket) and (Char =:= $B)) ->
    send_drv_reqs(Drv, Rs),
    case down_stack(save_line(Ls0, edlin:current_line(Cont))) of
	{none,_Ls} ->
	    send_drv(Drv, beep),
	    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls0, Encoding);
	{Lcs,Ls} ->
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    {more_chars,Ncont,Nrs} = edlin:start(edlin:prompt(Cont)),
	    send_drv_reqs(Drv, Nrs),
	    get_line1(edlin:edit_line1(lists:sublist(Lcs, 1, length(Lcs)-1),
				      Ncont),
		      Drv,
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
get_line1({undefined,{_A,Mode,Char},Cs,Cont,Rs}, Drv, Ls, Encoding)
  when ((Mode =:= none) and (Char =:= $\^R)) ->
    send_drv_reqs(Drv, Rs),
    %% drop current line, move to search mode. We store the current
    %% prompt ('N>') and substitute it with the search prompt.
    send_drv_reqs(Drv, edlin:erase_line(Cont)),
    put(search_quit_prompt, edlin:prompt(Cont)),
    Pbs = prompt_bytes("(search)`': ", Encoding),
    {more_chars,Ncont,Nrs} = edlin:start(Pbs, search),
    send_drv_reqs(Drv, Nrs),
    get_line1(edlin:edit_line1(Cs, Ncont), Drv, Ls, Encoding);
get_line1({expand, Before, Cs0, Cont,Rs}, Drv, Ls0, Encoding) ->
    send_drv_reqs(Drv, Rs),
    ExpandFun = get(expand_fun),
    {Found, Add, Matches} = ExpandFun(Before),
    case Found of
	no -> send_drv(Drv, beep);
	yes -> ok
    end,
    Cs1 = append(Add, Cs0, Encoding), %%XXX:PaN should this always be unicode?
    Cs = case Matches of
	     [] -> Cs1;
	     _ -> MatchStr = edlin_expand:format_matches(Matches),
		  send_drv(Drv, {put_chars, unicode, unicode:characters_to_binary(MatchStr,unicode)}),
		  [$\^L | Cs1]
	 end,
    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls0, Encoding);
get_line1({undefined,_Char,Cs,Cont,Rs}, Drv, Ls, Encoding) ->
    send_drv_reqs(Drv, Rs),
    send_drv(Drv, beep),
    get_line1(edlin:edit_line(Cs, Cont), Drv, Ls, Encoding);
%% The search item was found and accepted (new line entered on the exact
%% result found)
get_line1({_What,Cont={line,_Prompt,_Chars,search_found},Rs}, Drv, Ls0, Encoding) ->
    Line = edlin:current_line(Cont),
    %% this may create duplicate entries.
    Ls = save_line(new_stack(get_lines(Ls0)), Line),
    get_line1({done, Line, "", Rs}, Drv, Ls, Encoding);
%% The search mode has been exited, but the user wants to remain in line
%% editing mode wherever that was, but editing the search result.
get_line1({What,Cont={line,_Prompt,_Chars,search_quit},Rs}, Drv, Ls, Encoding) ->
    Line = edlin:current_chars(Cont),
    %% Load back the old prompt with the correct line number.
    case get(search_quit_prompt) of
	undefined -> % should not happen. Fallback.
	    LsFallback = save_line(new_stack(get_lines(Ls)), Line),
	    get_line1({done, "\n", Line, Rs}, Drv, LsFallback, Encoding);
	Prompt -> % redraw the line and keep going with the same stack position
	    NCont = {line,Prompt,{lists:reverse(Line),[]},none},
	    send_drv_reqs(Drv, Rs),
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    send_drv_reqs(Drv, edlin:redraw_line(NCont)),
	    get_line1({What, NCont ,[]}, Drv, pad_stack(Ls), Encoding)
    end;
%% Search mode is entered.
get_line1({What,{line,Prompt,{RevCmd0,_Aft},search},Rs},
       Drv, Ls0, Encoding) ->
    send_drv_reqs(Drv, Rs),
    %% Figure out search direction. ^S and ^R are returned through edlin
    %% whenever we received a search while being already in search mode.
    {Search, Ls1, RevCmd} = case RevCmd0 of
	[$\^S|RevCmd1] ->
	    {fun search_down_stack/2, Ls0, RevCmd1};
	[$\^R|RevCmd1] ->
	    {fun search_up_stack/2, Ls0, RevCmd1};
	_ -> % new search, rewind stack for a proper search.
	    {fun search_up_stack/2, new_stack(get_lines(Ls0)), RevCmd0}
    end,
    Cmd = lists:reverse(RevCmd),
    {Ls, NewStack} = case Search(Ls1, Cmd) of
	{none, Ls2} ->
	    send_drv(Drv, beep),
	    {Ls2, {RevCmd, "': "}};
	{Line, Ls2} -> % found. Complete the output edlin couldn't have done.
	    send_drv_reqs(Drv, [{put_chars, Encoding, Line}]),
	    {Ls2, {RevCmd, "': "++Line}}
    end,
    Cont = {line,Prompt,NewStack,search},
    more_data(What, Cont, Drv, Ls, Encoding);
get_line1({What,Cont0,Rs}, Drv, Ls, Encoding) ->
    send_drv_reqs(Drv, Rs),
    more_data(What, Cont0, Drv, Ls, Encoding).

more_data(What, Cont0, Drv, Ls, Encoding) ->
    receive
	{Drv,{data,Cs}} ->
	    get_line1(edlin:edit_line(Cs, Cont0), Drv, Ls, Encoding);
	{Drv,eof} ->
	    get_line1(edlin:edit_line(eof, Cont0), Drv, Ls, Encoding);
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
	    {more_chars,Cont,_More} = edlin:edit_line([], Cont0),
	    send_drv_reqs(Drv, edlin:erase_line(Cont)),
	    io_request(Req, From, ReplyAs, Drv, []), %WRONG!!!
	    send_drv_reqs(Drv, edlin:redraw_line(Cont)),
	    get_line1({more_chars,Cont,[]}, Drv, Ls, Encoding);
        {reply,{{From,ReplyAs},Reply}} ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
            more_data(What, Cont0, Drv, Ls, Encoding);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated
    after
	get_line_timeout(What)->
	    get_line1(edlin:edit_line([], Cont0), Drv, Ls, Encoding)
    end.

get_line_echo_off(Chars, Pbs, Drv) ->
    send_drv_reqs(Drv, [{put_chars, unicode,Pbs}]),
    get_line_echo_off1(edit_line(Chars,[]), Drv).

get_line_echo_off1({Chars,[]}, Drv) ->
    receive
	{Drv,{data,Cs}} ->
	    get_line_echo_off1(edit_line(Cs, Chars), Drv);
	{Drv,eof} ->
	    get_line_echo_off1(edit_line(eof, Chars), Drv);
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
	    io_request(Req, From, ReplyAs, Drv, []),
	    get_line_echo_off1({Chars,[]}, Drv);
        {reply,{{From,ReplyAs},Reply}} when From =/= undefined ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
            get_line_echo_off1({Chars,[]},Drv);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated
    end;
get_line_echo_off1({Chars,Rest}, _Drv) ->
    {done,lists:reverse(Chars),case Rest of done -> []; _ -> Rest end}.

%% We support line editing for the ICANON mode except the following
%% line editing characters, which already has another meaning in
%% echo-on mode (See Advanced Programming in the Unix Environment, 2nd ed,
%% Stevens, page 638):
%% - ^u in posix/icanon mode: erase-line, prefix-arg in edlin
%% - ^t in posix/icanon mode: status, transpose-char in edlin
%% - ^d in posix/icanon mode: eof, delete-forward in edlin
%% - ^r in posix/icanon mode: reprint (silly in echo-off mode :-))
%% - ^w in posix/icanon mode: word-erase (produces a beep in edlin)
edit_line(eof, Chars) ->
    {Chars,done};
edit_line([],Chars) ->
    {Chars,[]};
edit_line([$\r,$\n|Cs],Chars) ->
    {[$\n | Chars], remainder_after_nl(Cs)};
edit_line([NL|Cs],Chars) when NL =:= $\r; NL =:= $\n ->
    {[$\n | Chars], remainder_after_nl(Cs)};
edit_line([Erase|Cs],[]) when Erase =:= $\177; Erase =:= $\^H ->
    edit_line(Cs,[]);
edit_line([Erase|Cs],[_|Chars]) when Erase =:= $\177; Erase =:= $\^H ->
    edit_line(Cs,Chars);
edit_line([Char|Cs],Chars) ->
    edit_line(Cs,[Char|Chars]).

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
%get_lines({stack, U, {}, []}) ->
%    U;
%get_lines({stack, U, {}, D}) ->
%    tl(lists:reverse(D, U));
%get_lines({stack, U, L, D}) ->
%    get_lines({stack, U, {}, [L|D]}).

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
    save_line_buffer([Line|Lines]).

save_line_buffer(Lines) ->
    put(line_buffer, Lines).

search_up_stack(Stack, Substr) ->
    case up_stack(Stack) of
	{none,NewStack} -> {none,NewStack};
	{L, NewStack} ->
	    case string:str(L, Substr) of
		0 -> search_up_stack(NewStack, Substr);
		_ -> {string:strip(L,right,$\n), NewStack}
	    end
    end.

search_down_stack(Stack, Substr) ->
    case down_stack(Stack) of
	{none,NewStack} -> {none,NewStack};
	{L, NewStack} ->
	    case string:str(L, Substr) of
		0 -> search_down_stack(NewStack, Substr);
		_ -> {string:strip(L,right,$\n), NewStack}
	    end
    end.


%% This is get_line without line editing (except for backspace) and
%% without echo.
get_password_line(Chars, Drv) ->
    get_password1(edit_password(Chars,[]),Drv).

get_password1({Chars,[]}, Drv) ->
    receive
	{Drv,{data,Cs}} ->
	    get_password1(edit_password(Cs,Chars),Drv);
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
	    %send_drv_reqs(Drv, [{delete_chars, -length(Pbs)}]),
	    io_request(Req, From, ReplyAs, Drv, []), %WRONG!!!
	    %% I guess the reason the above line is wrong is that Buf is
	    %% set to []. But do we expect anything but plain output?

	    get_password1({Chars, []}, Drv);
        {reply,{{From,ReplyAs},Reply}} ->
            %% We take care of replies from puts here as well
            io_reply(From, ReplyAs, Reply),
            get_password1({Chars, []},Drv);
	{'EXIT',Drv,interrupt} ->
	    interrupted;
	{'EXIT',Drv,_} ->
	    terminated
    end;
get_password1({Chars,Rest},Drv) ->
    send_drv_reqs(Drv,[{put_chars, unicode, "\n"}]),
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

cast(L, binary,latin1) when is_list(L) ->
    list_to_binary(L);
cast(L, list, latin1) when is_list(L) ->
    binary_to_list(list_to_binary(L)); %% Exception if not bytes
cast(L, binary,unicode) when is_list(L) ->
    unicode:characters_to_binary(L,utf8);
cast(Other, _, _) ->
    Other.

append(B, L, latin1) when is_binary(B) ->
    binary_to_list(B)++L;
append(B, L, unicode) when is_binary(B) ->
    unicode:characters_to_list(B,utf8)++L;
append(L1, L2, _) when is_list(L1) ->
    L1++L2;
append(_Eof, L, _) ->
    L.
