%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(user).
-compile( [ inline, { inline_size, 100 } ] ).

%% Basic standard i/o server for user interface port.

-export([start/0, start/1, start_out/0]).
-export([interfaces/1]).

-define(NAME, user).

%% Internal exports
-export([server/1, server/2]).

%% Defines for control ops
-define(CTRL_OP_GET_WINSIZE,100).

%%
%% The basic server and start-up.
%%

start() ->
    start_port([eof,binary]).

start([Mod,Fun|Args]) ->
    %% Mod,Fun,Args should return a pid. That process is supposed to act
    %% as the io port.
    Pid = apply(Mod, Fun, Args),  % This better work!
    Id = spawn(?MODULE, server, [Pid]),
    register(?NAME, Id),
    Id.

start_out() ->
    %% Output-only version of start/0
    start_port([out,binary]).

start_port(PortSettings) ->
    Id = spawn(?MODULE,server,[{fd,0,1},PortSettings]),
    register(?NAME,Id),
    Id.

%% Return the pid of the shell process.
%% Note: We can't ask the user process for this info since it
%% may be busy waiting for data from the port.
interfaces(User) ->
    case process_info(User, dictionary) of
	{dictionary,Dict} ->
	    case lists:keysearch(shell, 1, Dict) of
		{value,Sh={shell,Shell}} when is_pid(Shell) ->
		    [Sh];
		_ ->
		    []
	    end;
	_ ->
	    []
    end.


server(Pid) when is_pid(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    run(Pid).

server(PortName,PortSettings) ->
    process_flag(trap_exit, true),
    Port = open_port(PortName,PortSettings),
    run(Port).

run(P) ->
    put(read_mode,list),
    put(unicode,false),
    case init:get_argument(noshell) of
	%% non-empty list -> noshell
	{ok, [_|_]} -> 
	    put(shell, noshell),
	    server_loop(P, queue:new());
	_ ->
	    group_leader(self(), self()),
	    catch_loop(P, start_init_shell())
    end.

catch_loop(Port, Shell) ->
    catch_loop(Port, Shell, queue:new()).

catch_loop(Port, Shell, Q) ->
    case catch server_loop(Port, Q) of
	new_shell ->
	    exit(Shell, kill),
	    catch_loop(Port, start_new_shell());
	{unknown_exit,{Shell,Reason},_} ->			% shell has exited
	    case Reason of
		normal ->
		    put_chars("*** ", Port, []);
		_ ->
		    put_chars("*** ERROR: ", Port, [])
	    end,
	    put_chars("Shell process terminated! ***\n", Port, []),
	    catch_loop(Port, start_new_shell());
	{unknown_exit,_,Q1} ->
	    catch_loop(Port, Shell, Q1);	     
	{'EXIT',R} ->
	    exit(R)
    end.

link_and_save_shell(Shell) ->
    link(Shell),
    put(shell, Shell),
    Shell.        

start_init_shell() ->
    link_and_save_shell(shell:start(init)).

start_new_shell() ->
    link_and_save_shell(shell:start()).

server_loop(Port, Q) ->
    receive
	{io_request,From,ReplyAs,Request} when is_pid(From) ->
	    server_loop(Port, do_io_request(Request, From, ReplyAs, Port, Q));
	{Port,{data,Bytes}} ->
	    case get(shell) of
		noshell ->
		    server_loop(Port, queue:snoc(Q, Bytes));
		_ ->
		    case contains_ctrl_g_or_ctrl_c(Bytes) of
			false ->
			    server_loop(Port, queue:snoc(Q, Bytes));
			_ ->
			    throw(new_shell)
		    end
	    end;
	{Port, eof} ->
	    put(eof, true),
	    server_loop(Port, Q);

	%% Ignore messages from port here.
	{'EXIT',Port,badsig} ->			% Ignore badsig errors
	    server_loop(Port, Q);
	{'EXIT',Port,What} ->			% Port has exited
	    exit(What);

	%% Check if shell has exited
	{'EXIT',SomePid,What} ->
	    case get(shell) of
		noshell ->
		    server_loop(Port, Q);	% Ignore
		_ ->
		    throw({unknown_exit,{SomePid,What},Q})
	    end;
	
	_Other ->				% Ignore other messages
	    server_loop(Port, Q)
    end.


get_fd_geometry(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_WINSIZE,[])) of
	List when is_list(List), length(List) =:= 8 -> 
	    <<W:32/native,H:32/native>> = list_to_binary(List),
	    {W,H};
	_ ->
	    error
    end.


%% NewSaveBuffer = io_request(Request, FromPid, ReplyAs, Port, SaveBuffer)

do_io_request(Req, From, ReplyAs, Port, Q0) ->
    case io_request(Req, Port, Q0) of
	{_Status,Reply,Q1} ->
	    io_reply(From, ReplyAs, Reply),
	    Q1;
	{exit,What} ->
	    send_port(Port, close),
	    exit(What)
    end.

%% New in R13B
%% Encoding option (unicode/latin1)
io_request({put_chars,unicode,Chars}, Port, Q) -> % Binary new in R9C
    put_chars(wrap_characters_to_binary(Chars,unicode,
					case get(unicode) of 
					    true -> unicode;
					    _ -> latin1
					end), Port, Q);
io_request({put_chars,unicode,Mod,Func,Args}, Port, Q) ->
    Result = case catch apply(Mod,Func,Args) of
		 Data when is_list(Data); is_binary(Data) ->
		     wrap_characters_to_binary(Data,unicode,
					       case get(unicode) of 
						   true -> unicode;
						   _ -> latin1
					       end);
		 Undef ->
		     Undef
	     end,
    put_chars(Result, Port, Q);
io_request({put_chars,latin1,Chars}, Port, Q) -> % Binary new in R9C
    Data = case get(unicode) of
	       true ->
		   unicode:characters_to_binary(Chars,latin1,unicode);
	       false ->
		   erlang:iolist_to_binary(Chars)
	   end,
    put_chars(Data, Port, Q);
io_request({put_chars,latin1,Mod,Func,Args}, Port, Q) ->
    Result = case catch apply(Mod,Func,Args) of
		 Data when is_list(Data); is_binary(Data) ->
		     unicode:characters_to_binary(Data,latin1,
						  case get(unicode) of 
						      true -> unicode;
						      _ -> latin1
						  end);
		 Undef ->
		     Undef
	     end,
    put_chars(Result, Port, Q);
io_request({get_chars,Enc,Prompt,N}, Port, Q) -> % New in R9C
    get_chars(Prompt, io_lib, collect_chars, N, Port, Q, Enc);
io_request({get_line,Enc,Prompt}, Port, Q) ->
    case get(read_mode) of
	binary ->
	    get_line_bin(Prompt,Port,Q,Enc);
	_ ->
	    get_chars(Prompt, io_lib, collect_line, [], Port, Q, Enc)
    end;
io_request({get_until,Enc,Prompt,M,F,As}, Port, Q) ->
    get_chars(Prompt, io_lib, get_until, {M,F,As}, Port, Q, Enc);
%%  End New in R13B
io_request(getopts, Port, Q) ->
    getopts(Port, Q);
io_request({setopts,Opts}, Port, Q) when is_list(Opts) ->
    setopts(Opts, Port, Q);
io_request({requests,Reqs}, Port, Q) ->
    io_requests(Reqs, {ok,ok,Q}, Port);

%% New in R12
io_request({get_geometry,columns},Port,Q) ->
    case get_fd_geometry(Port) of
	{W,_H} ->
	    {ok,W,Q};
	_ ->
	    {error,{error,enotsup},Q}
    end;
io_request({get_geometry,rows},Port,Q) ->
    case get_fd_geometry(Port) of
	{_W,H} ->
	    {ok,H,Q};
	_ ->
	    {error,{error,enotsup},Q}
    end;
%% BC with pre-R13 nodes
io_request({put_chars,Chars}, Port, Q) ->
    io_request({put_chars,latin1,Chars}, Port, Q);
io_request({put_chars,Mod,Func,Args}, Port, Q) ->
    io_request({put_chars,latin1,Mod,Func,Args}, Port, Q);
io_request({get_chars,Prompt,N}, Port, Q) -> 
    io_request({get_chars,latin1,Prompt,N}, Port, Q);
io_request({get_line,Prompt}, Port, Q) ->
    io_request({get_line,latin1,Prompt}, Port, Q);
io_request({get_until,Prompt,M,F,As}, Port, Q) ->
    io_request({get_until,latin1,Prompt,M,F,As}, Port, Q);

io_request(R, _Port, Q) ->                      %Unknown request
    {error,{error,{request,R}},Q}.		%Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, Port)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,_Res,Q}, Port) ->
    io_requests(Rs, io_request(R, Port, Q), Port);
io_requests([_|_], Error, _) ->
    Error;
io_requests([], Stat, _) ->
    Stat.

%% put_port(DeepList, Port)
%%  Take a deep list of characters, flatten and output them to the
%%  port.

put_port(List, Port) ->
    send_port(Port, {command, List}).

%% send_port(Port, Command)

send_port(Port, Command) ->
    Port ! {self(),Command}.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

%% put_chars
put_chars(Chars, Port, Q) when is_binary(Chars) ->
    put_port(Chars, Port),
    {ok,ok,Q};
put_chars(Chars, Port, Q) ->
    case catch list_to_binary(Chars) of
	Binary when is_binary(Binary) ->
	    put_chars(Binary, Port, Q);
	_ ->
	    {error,{error,put_chars},Q}
    end.

expand_encoding([]) ->
    [];
expand_encoding([latin1 | T]) ->
    [{encoding,latin1} | expand_encoding(T)];
expand_encoding([unicode | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([H|T]) ->
    [H|expand_encoding(T)].

%% setopts
setopts(Opts0,Port,Q) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{list,binary}], 
	       expand_encoding(Opts0))),
    case check_valid_opts(Opts) of
	true ->
	    do_setopts(Opts,Port,Q);
	false ->
	    {error,{error,enotsup},Q}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,_}|T]) ->
    check_valid_opts(T);
check_valid_opts([{encoding,Valid}|T]) when Valid =:= latin1; Valid =:= utf8; Valid =:= unicode ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

do_setopts(Opts, _Port, Q) ->
    case proplists:get_value(encoding,Opts) of
	Valid when Valid =:= unicode; Valid =:= utf8 ->
	    put(unicode,true);
	latin1 ->
	    put(unicode,false);
	undefined ->
	    ok
    end,
    case proplists:get_value(binary, Opts) of
	true ->
	    put(read_mode,binary),
	    {ok,ok,Q};
	false ->
	    put(read_mode,list),
	    {ok,ok,Q};
	_ ->
	    {ok,ok,Q}
    end.

getopts(_Port,Q) ->
    Bin = {binary, case get(read_mode) of
		       binary ->
			   true;
		       _ ->
			   false
		   end},
    Uni = {encoding, case get(unicode) of
		       true ->
			   unicode;
		       _ ->
			   latin1
		   end},
    {ok,[Bin,Uni],Q}.


get_line_bin(Prompt,Port,Q, Enc) ->
    prompt(Port, Prompt),
    case {get(eof),queue:is_empty(Q)} of
	{true,true} ->
	    {ok,eof,Q};
	_ ->
	    get_line(Prompt,Port, Q, [], Enc)
    end.
get_line(Prompt, Port, Q, Acc, Enc) ->
    case queue:is_empty(Q) of
	true ->
	    receive
		{Port,{data,Bytes}} ->
		    get_line_bytes(Prompt, Port, Q, Acc, Bytes, Enc);
		{Port, eof} ->
		    put(eof, true),
		    {ok, eof, []};
                {io_request,From,ReplyAs,{get_geometry,_}=Req} when is_pid(From) ->
                    do_io_request(Req, From, ReplyAs, Port, 
                                  queue:new()), 
                    %% No prompt.
                    get_line(Prompt, Port, Q, Acc, Enc);
		{io_request,From,ReplyAs,Request} when is_pid(From) ->
		    do_io_request(Request, From, ReplyAs, Port, queue:new()), 
		    prompt(Port, Prompt),
		    get_line(Prompt, Port, Q, Acc, Enc);
		{'EXIT',From,What} when node(From) =:= node() ->
		    {exit,What}
	    end;
	false ->
	    get_line_doit(Prompt, Port, Q, Acc, Enc)
    end.

get_line_bytes(Prompt, Port, Q, Acc, Bytes, Enc) ->
    case get(shell) of
	noshell ->
	    get_line_doit(Prompt, Port, queue:snoc(Q, Bytes),Acc,Enc);
	_ ->
	    case contains_ctrl_g_or_ctrl_c(Bytes) of
		false ->
		    get_line_doit(Prompt, Port, queue:snoc(Q, Bytes), Acc, Enc);
		_ ->
		    throw(new_shell)
	    end
    end.
is_cr_at(Pos,Bin) ->
    case Bin of
  	<<_:Pos/binary,$\r,_/binary>> ->
  	    true;
  	_ ->
  	    false
    end.
srch(<<>>,_,_) ->
    nomatch;
srch(<<X:8,_/binary>>,X,N) ->
    {match,[{N,1}]};
srch(<<_:8,T/binary>>,X,N) ->
    srch(T,X,N+1).
get_line_doit(Prompt, Port, Q, Accu, Enc) -> 
    case queue:is_empty(Q) of
	true ->
	    case get(eof) of
		true ->
		   case Accu of
		       [] ->
			   {ok,eof,Q};
		       _ ->
			   {ok,binrev(Accu,[]),Q}
		   end;
		_ ->
		    get_line(Prompt, Port, Q, Accu, Enc)
	    end;
	false ->
	    Bin = queue:head(Q),
	    case srch(Bin,$\n,0) of
		nomatch ->
		    X = byte_size(Bin)-1,
		    case is_cr_at(X,Bin) of
			true ->
			    <<D:X/binary,_/binary>> = Bin,
			    get_line_doit(Prompt, Port, queue:tail(Q), 
					  [<<$\r>>,D|Accu], Enc); 
			false ->
			    get_line_doit(Prompt, Port, queue:tail(Q), 
					  [Bin|Accu], Enc)
		    end;
		{match,[{Pos,1}]} ->
		    %% We are done
		    PosPlus = Pos + 1,
		    case Accu of
			[] ->
			    {Head,Tail} = 
				case is_cr_at(Pos - 1,Bin) of
				    false ->
					<<H:PosPlus/binary,
					 T/binary>> = Bin,
					{H,T};
				    true ->
					PosMinus = Pos - 1,
					<<H:PosMinus/binary,
					 _,_,T/binary>> = Bin,
					{binrev([],[H,$\n]),T}
				end,
			    case Tail of
				<<>> ->
				    {ok, cast(Head,Enc), queue:tail(Q)};
				_ ->
				    {ok, cast(Head,Enc), 
				     queue:cons(Tail, queue:tail(Q))}
			    end;
			[<<$\r>>|Stack1] when Pos =:= 0 ->
			    <<_:PosPlus/binary,Tail/binary>> = Bin, 
			    case Tail of
				<<>> ->
				    {ok, cast(binrev(Stack1, [$\n]),Enc), 
				     queue:tail(Q)};
				_ ->
				    {ok, cast(binrev(Stack1, [$\n]),Enc), 
				     queue:cons(Tail, queue:tail(Q))}
			    end;
			_ ->
			    {Head,Tail} = 
				case is_cr_at(Pos - 1,Bin) of
				    false ->
					<<H:PosPlus/binary,
					 T/binary>> = Bin,
					{H,T};
				    true ->
					PosMinus = Pos - 1,
					<<H:PosMinus/binary,
					 _,_,T/binary>> = Bin,
					{[H,$\n],T}
				end,
			    case Tail of
				<<>> ->
				    {ok, cast(binrev(Accu,[Head]),Enc), 
				     queue:tail(Q)};
				_ ->
				    {ok, cast(binrev(Accu,[Head]),Enc), 
				     queue:cons(Tail, queue:tail(Q))}
			    end
		    end
	    end
    end.

binrev(L, T) ->
    list_to_binary(lists:reverse(L, T)).

%%  is_cr_at(Pos,Bin) ->
%%      case Bin of
%%  	<<_:Pos/binary,$\r,_/binary>> ->
%%  	    true;
%%  	_ ->
%%  	    false
%%      end.

%%  collect_line_bin_re(Bin,_Data,Stack,_) ->
%%      case re:run(Bin,<<"\n">>) of
%%  	nomatch ->
%%  	    X = byte_size(Bin)-1,
%%  	    case is_cr_at(X,Bin) of
%%  		true ->
%%  		    <<D:X/binary,_/binary>> = Bin,
%%  		    [<<$\r>>,D|Stack]; 
%%  		false ->
%%  		    [Bin|Stack]
%%  	    end;
%%  	{match,[{Pos,1}]} ->
%%  	    PosPlus = Pos + 1,
%%  	    case Stack of
%%  		[] ->
%%  		    case is_cr_at(Pos - 1,Bin) of
%%  			false ->
%%  			    <<Head:PosPlus/binary,Tail/binary>> = Bin, 
%%  			    {stop, Head, Tail};
%%  			true ->
%%  			    PosMinus = Pos - 1,
%%  			    <<Head:PosMinus/binary,_,_,Tail/binary>> = Bin,
%%  			    {stop, binrev([],[Head,$\n]),Tail}
%%  		    end;
%%  		[<<$\r>>|Stack1] when Pos =:= 0 ->

%%  		    <<_:PosPlus/binary,Tail/binary>> = Bin, 
%%  		    {stop,binrev(Stack1, [$\n]),Tail};
%%  		_ ->
%%  		    case is_cr_at(Pos - 1,Bin) of
%%  			false ->
%%  			    <<Head:PosPlus/binary,Tail/binary>> = Bin, 
%%  			    {stop,binrev(Stack, [Head]),Tail};
%%  			true ->
%%  			    PosMinus = Pos - 1,
%%  			    <<Head:PosMinus/binary,_,_,Tail/binary>> = Bin,
%%  			    {stop, binrev(Stack,[Head,$\n]),Tail}
%%  		    end
%%  	    end
%%      end.
%% get_chars(Prompt, Module, Function, XtraArg, Port, Queue)
%%  Gets characters from the input port until the applied function
%%  returns {stop,Result,RestBuf}. Does not block output until input 
%%  has been received.
%%  Returns:
%%	{Status,Result,NewQueue}
%%	{exit,Reason}
    
%% Entry function.
get_chars(Prompt, M, F, Xa, Port, Q, Fmt) ->
    prompt(Port, Prompt),
    case {get(eof),queue:is_empty(Q)} of
	{true,true} ->
	    {ok,eof,Q};
	_ ->
	    get_chars(Prompt, M, F, Xa, Port, Q, start, Fmt)
    end.

%% First loop. Wait for port data. Respond to output requests.
get_chars(Prompt, M, F, Xa, Port, Q, State, Fmt) ->
    case queue:is_empty(Q) of
	true ->
	    receive
		{Port,{data,Bytes}} ->
		    get_chars_bytes(State, M, F, Xa, Port, Q, Bytes, Fmt);
		{Port, eof} ->
		    put(eof, true),
		    {ok, eof, []};
		%%{io_request,From,ReplyAs,Request} when is_pid(From) ->
		%%    get_chars_req(Prompt, M, F, Xa, Port, queue:new(), State,
		%%		  Request, From, ReplyAs);
                {io_request,From,ReplyAs,{get_geometry,_}=Req} when is_pid(From) ->
                    do_io_request(Req, From, ReplyAs, Port, 
                                  queue:new()), %Keep Q over this call
                    %% No prompt.
                    get_chars(Prompt, M, F, Xa, Port, Q, State, Fmt);
		{io_request,From,ReplyAs,Request} when is_pid(From) ->
		    get_chars_req(Prompt, M, F, Xa, Port, Q, State,
				  Request, From, ReplyAs, Fmt);
		{'EXIT',From,What} when node(From) =:= node() ->
		    {exit,What}
	    end;
	false ->
	    get_chars_apply(State, M, F, Xa, Port, Q, Fmt)
    end.

get_chars_req(Prompt, M, F, XtraArg, Port, Q, State,
	      Req, From, ReplyAs, Fmt) ->
    do_io_request(Req, From, ReplyAs, Port, queue:new()), %Keep Q over this call
    prompt(Port, Prompt),
    get_chars(Prompt, M, F, XtraArg, Port, Q, State, Fmt).

%% Second loop. Pass data to client as long as it wants more.
%% A ^G in data interrupts loop if 'noshell' is not undefined.
get_chars_bytes(State, M, F, Xa, Port, Q, Bytes, Fmt) ->
    case get(shell) of
	noshell ->
	    get_chars_apply(State, M, F, Xa, Port, queue:snoc(Q, Bytes),Fmt);
	_ ->
	    case contains_ctrl_g_or_ctrl_c(Bytes) of
		false ->
		    get_chars_apply(State, M, F, Xa, Port, 
				    queue:snoc(Q, Bytes),Fmt);
		_ ->
		    throw(new_shell)
	    end
    end.

get_chars_apply(State0, M, F, Xa, Port, Q, Fmt) ->
    case catch M:F(State0, cast(queue:head(Q),Fmt), Fmt, Xa) of
	{stop,Result,<<>>} ->
	    {ok,Result,queue:tail(Q)};
	{stop,Result,[]} ->
	    {ok,Result,queue:tail(Q)};
	{stop,Result,eof} ->
	    {ok,Result,queue:tail(Q)};
	{stop,Result,Buf} ->
	    {ok,Result,queue:cons(Buf, queue:tail(Q))};
	{'EXIT',_} ->
	    {error,{error,err_func(M, F, Xa)},queue:new()};
	State1 ->
	    get_chars_more(State1, M, F, Xa, Port, queue:tail(Q), Fmt)
    end.

get_chars_more(State, M, F, Xa, Port, Q, Fmt) ->
    case queue:is_empty(Q) of
	true ->
	    case get(eof) of
		undefined ->
		    receive
			{Port,{data,Bytes}} ->
			    get_chars_bytes(State, M, F, Xa, Port, Q, Bytes, Fmt);
			{Port,eof} ->
			    put(eof, true),
			    get_chars_apply(State, M, F, Xa, Port, 
					    queue:snoc(Q, eof), Fmt);
			{'EXIT',From,What} when node(From) =:= node() ->
			    {exit,What}
		    end;
		_ ->
		    get_chars_apply(State, M, F, Xa, Port, queue:snoc(Q, eof), Fmt)
	    end;
	false ->
	    get_chars_apply(State, M, F, Xa, Port, Q, Fmt)
    end.


%% prompt(Port, Prompt)
%%  Print Prompt onto Port

%% common case, reduces execution time by 20%
prompt(_Port, '') -> ok;

prompt(Port, Prompt) ->
    put_port(io_lib:format_prompt(Prompt), Port).

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.

%% using regexp reduces execution time by >50% compared to old code
%% running two regexps in sequence is much faster than \\x03|\\x07
contains_ctrl_g_or_ctrl_c(BinOrList)->
    case {re:run(BinOrList, <<3>>),re:run(BinOrList, <<7>>)} of
	{nomatch, nomatch} -> false;
	_ -> true
    end.

%% Convert a buffer between list and binary
cast(Data, _Format) when is_atom(Data) ->
    Data;
cast(Data, Format) ->
    cast(Data, get(read_mode), Format, get(unicode)).

cast(B, binary, latin1, false) when is_binary(B) ->
    B;
cast(B, binary, latin1, true) when is_binary(B) ->
    unicode:characters_to_binary(B, unicode, latin1);
cast(L, binary, latin1, false) ->
    erlang:iolist_to_binary(L);
cast(L, binary, latin1, true) ->
    case unicode:characters_to_binary(
	   erlang:iolist_to_binary(L),unicode,latin1) of % may fail
	{error,_,_} -> exit({no_translation, unicode, latin1});
	Else -> Else
    end;
cast(B, binary, unicode, true) when is_binary(B) ->
    B;
cast(B, binary, unicode, false) when is_binary(B) ->
    unicode:characters_to_binary(B,latin1,unicode);
cast(L, binary, unicode, true) ->
    % possibly a list containing UTF-8 encoded characters
    unicode:characters_to_binary(erlang:iolist_to_binary(L));
cast(L, binary, unicode, false) ->
    unicode:characters_to_binary(L, latin1, unicode);
cast(L, list, latin1, UniTerm) ->
    case UniTerm of
	true -> % Convert input characters to protocol format (i.e latin1)
	    case unicode:characters_to_list(
		   erlang:iolist_to_binary(L),unicode) of % may fail
		{error,_,_} -> exit({no_translation, unicode, latin1});
		Else -> [ case X of 
			      High when High > 255 -> 
				  exit({no_translation, unicode, latin1}); 
			      Low -> 
				  Low 
			  end || X <- Else ]
	    end;
	_ ->
	    binary_to_list(erlang:iolist_to_binary(L))
    end;
cast(L, list, unicode, UniTerm)  ->
    unicode:characters_to_list(erlang:iolist_to_binary(L),
			       case UniTerm of 
				   true -> unicode; 
				   _ -> latin1 
			       end);
cast(Other, _, _,_) ->
    Other.

wrap_characters_to_binary(Chars,unicode,latin1) ->
    case unicode:characters_to_binary(Chars,unicode,latin1) of
	{error,_,_} -> 
	    list_to_binary( 
	      [ case X of
		    High when High > 255 ->
			["\\x{",erlang:integer_to_list(X, 16),$}];
		    Low ->
			Low
		end || X <- unicode:characters_to_list(Chars,unicode) ]);
	Bin ->
	    Bin
    end;
		       
wrap_characters_to_binary(Bin,From,From) when is_binary(Bin) ->
    Bin;
wrap_characters_to_binary(Chars,From,To) ->
    unicode:characters_to_binary(Chars,From,To).
