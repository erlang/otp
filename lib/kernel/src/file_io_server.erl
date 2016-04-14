%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2015. All Rights Reserved.
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
-module(file_io_server).

%% A simple file server for io to one file instance per server instance.

-export([format_error/1]).
-export([start/3, start_link/3]).

-export([count_and_find/3]).

-record(state, {handle,owner,mref,buf,read_mode,unic}).

-define(PRIM_FILE, prim_file).
-define(READ_SIZE_LIST, 128).
-define(READ_SIZE_BINARY, (8*1024)).

-define(eat_message(M, T), receive M -> M after T -> timeout end).

%%%-----------------------------------------------------------------
%%% Exported functions

format_error({_Line, ?MODULE, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({_Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(invalid_unicode) ->
    io_lib:format("cannot translate from UTF-8", []);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

start(Owner, FileName, ModeList) 
  when is_pid(Owner), (is_list(FileName) orelse is_binary(FileName)), is_list(ModeList) ->
    do_start(spawn, Owner, FileName, ModeList).

start_link(Owner, FileName, ModeList) 
  when is_pid(Owner), (is_list(FileName) orelse is_binary(FileName)), is_list(ModeList) ->
    do_start(spawn_link, Owner, FileName, ModeList).

%%%-----------------------------------------------------------------
%%% Server starter, dispatcher and helpers

do_start(Spawn, Owner, FileName, ModeList) ->
    Self = self(),
    Ref = make_ref(),
    Utag = erlang:dt_spread_tag(true),
    Pid = 
	erlang:Spawn(
	  fun() ->
		  erlang:dt_restore_tag(Utag),
		  %% process_flag(trap_exit, true),
		  case parse_options(ModeList) of
		      {ReadMode, UnicodeMode, Opts} ->
			  case ?PRIM_FILE:open(FileName, Opts) of
			      {error, Reason} = Error ->
				  Self ! {Ref, Error},
				  exit(Reason);
			      {ok, Handle} ->
				  %% XXX must I handle R6 nodes here?
				  M = erlang:monitor(process, Owner),
				  Self ! {Ref, ok},
				  server_loop(
				    #state{handle    = Handle,
					   owner     = Owner, 
					   mref      = M, 
					   buf       = <<>>,
					   read_mode = ReadMode,
					   unic = UnicodeMode})
			  end;
		      {error,Reason1} = Error1 ->
			  Self ! {Ref, Error1},
			  exit(Reason1)
		  end
	  end),
    erlang:dt_restore_tag(Utag),
    Mref = erlang:monitor(process, Pid),
    receive
	{Ref, {error, _Reason} = Error} ->
	    erlang:demonitor(Mref, [flush]),
	    Error;
	{Ref, ok} ->
	    erlang:demonitor(Mref),
	    receive
		{'DOWN', Mref, _, _, Reason} ->
		    {error, Reason}
	    after 0 ->
		    {ok, Pid}
	    end;
	{'DOWN', Mref, _, _, Reason} ->
	    {error, Reason}
    end.

%%% Returns {ReadMode, UnicodeMode, RealOpts}
parse_options(List) ->
    parse_options(expand_encoding(List), list, latin1, []).

parse_options([], list, Uni, Acc) ->
    {list,Uni,[binary|lists:reverse(Acc)]};
parse_options([], binary, Uni, Acc) ->
    {binary,Uni,lists:reverse(Acc)};
parse_options([{encoding, Encoding}|T], RMode, _, Acc) ->
    case valid_enc(Encoding) of 
	{ok, ExpandedEnc} ->
	    parse_options(T, RMode, ExpandedEnc, Acc);
	{error,_Reason} = Error ->
	    Error
    end;
parse_options([binary|T], _, Uni, Acc) ->
    parse_options(T, binary, Uni, [binary|Acc]);
parse_options([H|T], R, U, Acc) ->
    parse_options(T, R, U, [H|Acc]).

expand_encoding([]) ->
    [];
expand_encoding([latin1 | T]) ->
    [{encoding,latin1} | expand_encoding(T)];
expand_encoding([unicode | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([H|T]) ->
    [H|expand_encoding(T)].

valid_enc(latin1) ->
    {ok,latin1};
valid_enc(utf8) ->
    {ok,unicode};
valid_enc(unicode) ->
    {ok,unicode};
valid_enc(utf16) ->
    {ok,{utf16,big}};
valid_enc({utf16,big}) ->
    {ok,{utf16,big}};
valid_enc({utf16,little}) ->
    {ok,{utf16,little}};
valid_enc(utf32) ->
    {ok,{utf32,big}};
valid_enc({utf32,big}) ->
    {ok,{utf32,big}};
valid_enc({utf32,little}) ->
    {ok,{utf32,little}};
valid_enc(_Other) ->
    {error,badarg}.


server_loop(#state{mref = Mref} = State) ->
    receive
	{file_request, From, ReplyAs, Request} when is_pid(From) ->
	    case file_request(Request, State) of
		{reply, Reply, NewState} ->
		    _ = file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    _ = file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    _ = file_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{io_request, From, ReplyAs, Request} when is_pid(From) ->
	    case io_request(Request, State) of
		{reply, Reply, NewState} ->
		    _ = io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    _ = io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    _ = io_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason);
	_ ->
	    server_loop(State)
    end.

file_reply(From, ReplyAs, Reply) ->
    From ! {file_reply, ReplyAs, Reply}.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%%%-----------------------------------------------------------------
%%% file requests

file_request({advise,Offset,Length,Advise},
         #state{handle=Handle}=State) ->
    case ?PRIM_FILE:advise(Handle, Offset, Length, Advise) of
    {error,Reason}=Reply ->
        {stop,Reason,Reply,State};
    Reply ->
        {reply,Reply,State}
    end;
file_request({allocate, Offset, Length},
         #state{handle = Handle} = State) ->
    Reply = ?PRIM_FILE:allocate(Handle, Offset, Length),
    {reply, Reply, State};
file_request({pread,At,Sz}, State)
  when At =:= cur;
       At =:= {cur,0} ->
    case get_chars(Sz, latin1, State) of
	{reply,Reply,NewState}
	  when is_list(Reply);
	       is_binary(Reply) ->
	    {reply,{ok,Reply},NewState};
	Other ->
	    Other
    end;
file_request({pread,At,Sz}, 
	     #state{handle=Handle,buf=Buf}=State) ->
    case position(Handle, At, Buf) of
	{error,_} = Reply ->
	    {error,Reply,State};
	_ ->
	    case get_chars(Sz, latin1, State#state{buf= <<>>}) of
		{reply,Reply,NewState}
		  when is_list(Reply);
		       is_binary(Reply) ->
		    {reply,{ok,Reply},NewState};
		Other ->
		    Other
	    end
    end;
file_request({pwrite,At,Data},
	     #state{buf= <<>>}=State)
  when At =:= cur;
       At =:= {cur,0} ->
    put_chars(Data, latin1, State);
file_request({pwrite,At,Data}, 
	     #state{handle=Handle,buf=Buf}=State) ->
    case position(Handle, At, Buf) of
	{error,_} = Reply ->
	    {error,Reply,State};
	_ ->
	    put_chars(Data, latin1, State)
    end;
file_request(datasync,
	     #state{handle=Handle}=State) ->
    case ?PRIM_FILE:datasync(Handle) of
	{error,Reason}=Reply ->
	    {stop,Reason,Reply,State};
	Reply ->
	    {reply,Reply,State}
    end;
file_request(sync, 
	     #state{handle=Handle}=State) ->
    case ?PRIM_FILE:sync(Handle) of
	{error,Reason}=Reply ->
	    {stop,Reason,Reply,State};
	Reply ->
	    {reply,Reply,State}
    end;
file_request(close, 
	     #state{handle=Handle}=State) ->
    case ?PRIM_FILE:close(Handle) of
	{error,Reason}=Reply ->
	    {stop,Reason,Reply,State#state{buf= <<>>}};
	Reply ->
	    {stop,normal,Reply,State#state{buf= <<>>}}
    end;
file_request({position,At}, 
	     #state{handle=Handle,buf=Buf}=State) ->
    case position(Handle, At, Buf) of
	{error,_} = Reply ->
	    {error,Reply,State};
	Reply ->
	    std_reply(Reply, State)
    end;
file_request(truncate, 
	     #state{handle=Handle}=State) ->
    case ?PRIM_FILE:truncate(Handle) of
	{error,Reason}=Reply ->
	    {stop,Reason,Reply,State#state{buf= <<>>}};
	Reply ->
	    std_reply(Reply, State)
    end;
file_request(Unknown, 
	     #state{}=State) ->
    Reason = {request, Unknown},
    {error,{error,Reason},State}.

%% Standard reply and clear buffer
std_reply({error,_}=Reply, State) ->
    {error,Reply,State#state{buf= <<>>}};
std_reply(Reply, State) ->
    {reply,Reply,State#state{buf= <<>>}}.

%%%-----------------------------------------------------------------
%%% I/O request 

%% New protocol with encoding tags (R13)
io_request({put_chars, Enc, Chars}, 
	   #state{buf= <<>>}=State) ->
    put_chars(Chars, Enc, State);
io_request({put_chars, Enc, Chars}, 
	   #state{handle=Handle,buf=Buf}=State) ->
    case position(Handle, cur, Buf) of
	{error,Reason}=Reply ->
	    {stop,Reason,Reply,State};
	_ ->
	    put_chars(Chars, Enc, State#state{buf= <<>>})
    end;
io_request({put_chars,Enc,Mod,Func,Args}, 
	   #state{}=State) ->
    case catch apply(Mod, Func, Args) of
	Chars when is_list(Chars); is_binary(Chars) ->
	    io_request({put_chars,Enc,Chars}, State);
	_ ->
	    {error,{error,Func},State}
    end;


io_request({get_until,Enc,_Prompt,Mod,Func,XtraArgs}, 
	   #state{}=State) ->
    get_chars(io_lib, get_until, {Mod, Func, XtraArgs}, Enc, State);
io_request({get_chars,Enc,_Prompt,N}, 
	   #state{}=State) ->
    get_chars(N, Enc, State);

io_request({get_line,OutEnc,_Prompt}, #state{buf=Buf, read_mode=Mode, unic=InEnc} = State0) ->
    try
	%% Minimize the encoding conversions
	WorkEnc = case InEnc of
		      {_,_} -> OutEnc; %% utf16 or utf32
		      _ -> InEnc %% Byte oriented utf8 or latin1
		  end,
	{Res, State} = get_line(start, convert_enc(Buf, InEnc, WorkEnc), WorkEnc, State0),
	{reply, cast(Res, Mode, WorkEnc, OutEnc), State}
    catch exit:ExError ->
	    {stop,ExError,{error,ExError},State0#state{buf= <<>>}}
    end;

io_request({setopts, Opts}, 
	   #state{}=State) when is_list(Opts) ->
    setopts(Opts, State);

io_request(getopts, 
	   #state{}=State) ->
    getopts(State);

%% BC with pre-R13 nodes
io_request({put_chars, Chars},#state{}=State) ->
    io_request({put_chars, latin1, Chars},State);
io_request({put_chars,Mod,Func,Args}, #state{}=State) ->
    io_request({put_chars,latin1,Mod,Func,Args}, State);
io_request({get_until,_Prompt,Mod,Func,XtraArgs}, #state{}=State) ->
    io_request({get_until,latin1,_Prompt,Mod,Func,XtraArgs}, State);
io_request({get_chars,_Prompt,N}, #state{}=State) ->
    io_request({get_chars,latin1,_Prompt,N}, State);
io_request({get_line,_Prompt}, #state{}=State) ->
    io_request({get_line,latin1,_Prompt}, State);

io_request({requests,Requests}, 
	   #state{}=State) when is_list(Requests) ->
    io_request_loop(Requests, {reply,ok,State});
io_request(Unknown, 
	   #state{}=State) ->
    Reason = {request,Unknown},
    {error,{error,Reason},State}.


%% Process a list of requests as long as the results are ok.

io_request_loop([], Result) ->
    Result;
io_request_loop([_Request|_Tail], 
		{stop,_Reason,_Reply,_State}=Result) ->
    Result;
io_request_loop([_Request|_Tail],
		{error,_Reply,_State}=Result) ->
    Result;
io_request_loop([Request|Tail], 
		{reply,_Reply,State}) ->
    io_request_loop(Tail, io_request(Request, State)).


%% I/O request put_chars
%%
put_chars(Chars, latin1, #state{handle=Handle, unic=latin1}=State) ->
    NewState = State#state{buf = <<>>},
    case ?PRIM_FILE:write(Handle, Chars) of
	{error,Reason}=Reply ->
	    {stop,Reason,Reply,NewState};
	Reply ->
	    {reply,Reply,NewState}
    end;
put_chars(Chars, InEncoding, #state{handle=Handle, unic=OutEncoding}=State) ->
    NewState = State#state{buf = <<>>},
    case unicode:characters_to_binary(Chars,InEncoding,OutEncoding) of
	Bin when is_binary(Bin) ->
	    case ?PRIM_FILE:write(Handle, Bin) of
		{error,Reason}=Reply ->
		    {stop,Reason,Reply,NewState};
		Reply ->
		    {reply,Reply,NewState}
	    end;
	{error,_,_} ->
	    {stop,no_translation,
	     {error,{no_translation, InEncoding, OutEncoding}},
	     NewState}
    end.

get_line(S, {<<>>, Cont}, OutEnc,
	 #state{handle=Handle, read_mode=Mode, unic=InEnc}=State) ->
    case ?PRIM_FILE:read(Handle, read_size(Mode)) of
	{ok,Bin} ->
	    get_line(S, convert_enc([Cont, Bin], InEnc, OutEnc), OutEnc, State);
	eof ->
	    get_line(S, {eof, Cont}, OutEnc, State);
	{error,Reason}=Error ->
	    {stop,Reason,Error,State}
    end;
get_line(S0, {Buf, BCont}, OutEnc, #state{unic=InEnc}=State) ->
    case io_lib:collect_line(S0, Buf, OutEnc, []) of
	{stop, Result, Cont0} ->
	    %% Convert both buffers back to file InEnc encoding
	    {Cont, <<>>} = convert_enc(Cont0, OutEnc, InEnc),
	    {Result, State#state{buf=cast_binary([Cont, BCont])}};
	S ->
	    get_line(S, {<<>>, BCont}, OutEnc, State)
    end.

convert_enc(Bins, Enc, Enc) ->
    {cast_binary(Bins), <<>>};
convert_enc(eof, _, _) ->
    {<<>>, <<>>};
convert_enc(Bin, InEnc, OutEnc) ->
    case unicode:characters_to_binary(Bin, InEnc, OutEnc) of
	Res when is_binary(Res) ->
	    {Res, <<>>};
	{incomplete, Res, Cont} ->
	    {Res, Cont};
	{error, _, _} ->
	    exit({no_translation, InEnc, OutEnc})
    end.

%%    
%% Process the I/O request get_chars
%%
get_chars(0, Enc, #state{read_mode=ReadMode,unic=InEncoding}=State) ->
    {reply,cast(<<>>, ReadMode,InEncoding, Enc),State};
get_chars(N, Enc, #state{buf=Buf,read_mode=ReadMode,unic=latin1}=State) 
  when is_integer(N), N > 0, N =< byte_size(Buf) ->
    {B1,B2} = split_binary(Buf, N),
    {reply,cast(B1, ReadMode,latin1,Enc),State#state{buf=B2}};
get_chars(N, Enc, #state{buf=Buf,read_mode=ReadMode,unic=latin1}=State) 
  when is_integer(N), N > 0, N =< byte_size(Buf) ->
    {B1,B2} = split_binary(Buf, N),
    {reply,cast(B1, ReadMode,latin1,Enc),State#state{buf=B2}};
get_chars(N, OutEnc,#state{handle=Handle,buf=Buf,read_mode=ReadMode,unic=latin1}=State) 
  when is_integer(N), N > 0 ->
    BufSize = byte_size(Buf),
    NeedSize = N-BufSize,
    Size = erlang:max(NeedSize, ?READ_SIZE_BINARY),
    case ?PRIM_FILE:read(Handle, Size) of
	{ok, B} ->
	    if BufSize+byte_size(B) < N ->
		    std_reply(cat(Buf, B, ReadMode,latin1,OutEnc), State);
	       true ->
		    {B1,B2} = split_binary(B, NeedSize),
		    {reply,cat(Buf, B1, ReadMode, latin1,OutEnc),State#state{buf=B2}}
	    end;
	eof when BufSize =:= 0 ->
	    {reply,eof,State};
	eof ->
	    std_reply(cast(Buf, ReadMode,latin1,OutEnc), State);
	{error,Reason}=Error ->
	    {stop,Reason,Error,State#state{buf= <<>>}}
    end;
get_chars(N, OutEnc,#state{handle=Handle,buf=Buf,read_mode=ReadMode,unic=InEncoding}=State) 
  when is_integer(N), N > 0 ->
    try
	%% This is rather tricky, we need to count the actual number of characters 
	%% in the buffer first as unicode characters are not constant in length
	{BufCount, SplitPos} = count_and_find(Buf,N,InEncoding),
	case BufCount >= N of
	    true ->
		{B1,B2} = case SplitPos of
			      none -> {Buf,<<>>};
			      _ ->split_binary(Buf,SplitPos)
			  end,
		{reply,cast(B1, ReadMode,InEncoding,OutEnc),State#state{buf=B2}};
	    false ->
		%% Need more, Try to read 4*needed in bytes...
		NeedSize = (N - BufCount) * 4,
		Size = erlang:max(NeedSize, ?READ_SIZE_BINARY),
		case ?PRIM_FILE:read(Handle, Size) of
		    {ok, B} ->
			NewBuf = list_to_binary([Buf,B]),
			{NewCount,NewSplit} = count_and_find(NewBuf,N,InEncoding),
			case NewCount >= N of
			    true ->
				{B01,B02} = case NewSplit of
						none -> {NewBuf,<<>>};
						_ ->split_binary(NewBuf, NewSplit)
					    end,
				{reply,cast(B01, ReadMode,InEncoding,OutEnc),
				 State#state{buf=B02}};
			    false ->
				%% Reached end of file
				std_reply(cast(NewBuf, ReadMode,InEncoding,OutEnc), 
					  State#state{buf = <<>>})
			end;
		    eof when BufCount =:= 0 ->
			{reply,eof,State};
		    eof ->
			std_reply(cast(Buf, ReadMode,InEncoding,OutEnc), State#state{buf = <<>>});
		    {error,Reason}=Error ->
			{stop,Reason,Error,State#state{buf = <<>>}}
		end
	end
    catch
	exit:ExError ->
	    {stop,ExError,{error,ExError},State#state{buf= <<>>}}
    end;

get_chars(_N, _, #state{}=State) ->
    {error,{error,get_chars},State}.

get_chars(Mod, Func, XtraArg, OutEnc, #state{buf= <<>>}=State) ->
    get_chars_empty(Mod, Func, XtraArg, start, OutEnc, State);
get_chars(Mod, Func, XtraArg, OutEnc, #state{buf=Buf}=State) ->
    get_chars_apply(Mod, Func, XtraArg, start, OutEnc, State#state{buf= <<>>}, Buf).

get_chars_empty(Mod, Func, XtraArg, S, latin1,
		#state{handle=Handle,read_mode=ReadMode, unic=latin1}=State) ->
    case ?PRIM_FILE:read(Handle, read_size(ReadMode)) of
	{ok,Bin} ->
	    get_chars_apply(Mod, Func, XtraArg, S, latin1, State, Bin);
	eof ->
	    get_chars_apply(Mod, Func, XtraArg, S, latin1, State, eof);
	{error,Reason}=Error ->
	    {stop,Reason,Error,State}
    end;
get_chars_empty(Mod, Func, XtraArg, S, OutEnc,
		#state{handle=Handle,read_mode=ReadMode}=State) ->
    case ?PRIM_FILE:read(Handle, read_size(ReadMode)) of
	{ok,Bin} ->
	    get_chars_apply(Mod, Func, XtraArg, S, OutEnc, State, Bin);
	eof ->
	    get_chars_apply(Mod, Func, XtraArg, S, OutEnc, State, eof);
	{error,Reason}=Error ->
	    {stop,Reason,Error,State}
    end.
get_chars_notempty(Mod, Func, XtraArg, S, OutEnc,
		   #state{handle=Handle,read_mode=ReadMode,buf = B}=State) ->
    case ?PRIM_FILE:read(Handle, read_size(ReadMode)) of
	{ok,Bin} ->
	    get_chars_apply(Mod, Func, XtraArg, S, OutEnc, State, list_to_binary([B,Bin]));
	eof ->
	    case B of
		<<>> ->
		    get_chars_apply(Mod, Func, XtraArg, S, OutEnc, State, eof);
		_ ->
                    {stop,invalid_unicode,invalid_unicode_error(Mod, Func, XtraArg, S),State}
	    end;
	{error,Reason}=Error ->
	    {stop,Reason,Error,State}
    end.


get_chars_apply(Mod, Func, XtraArg, S0, latin1,
		#state{read_mode=ReadMode,unic=latin1}=State, Data0) ->
    Data1 = case ReadMode of
	       list when is_binary(Data0) -> binary_to_list(Data0);
	       _ -> Data0
	    end,
    case catch Mod:Func(S0, Data1, latin1, XtraArg) of
	{stop,Result,Buf} ->
	    {reply,Result,State#state{buf=cast_binary(Buf)}};
	{'EXIT',Reason} ->
	    {stop,Reason,{error,err_func(Mod, Func, XtraArg)},State};
	S1 ->
	    get_chars_empty(Mod, Func, XtraArg, S1, latin1, State)
    end;
get_chars_apply(Mod, Func, XtraArg, S0, OutEnc,
		#state{read_mode=ReadMode,unic=InEnc}=State, Data0) ->
    try 
	{Data1,NewBuff} = case ReadMode of
			      list when is_binary(Data0) -> 
				  case unicode:characters_to_list(Data0,InEnc) of
				      {Tag,Decoded,Rest} when Decoded =/= [], Tag =:= error; Decoded =/= [], Tag =:= incomplete ->
					  {Decoded,erlang:iolist_to_binary(Rest)};
				      {error, [], _}  -> 
					  exit(invalid_unicode);
				      {incomplete, [], R}  -> 
					  {[],R};
				      List when is_list(List) ->
					  {List,<<>>}
				  end;
			      binary when is_binary(Data0) ->
				  case unicode:characters_to_binary(Data0,InEnc,OutEnc) of
				      {Tag2,Decoded2,Rest2} when Decoded2 =/= <<>>, Tag2 =:= error; Decoded2 =/= <<>>, Tag2 =:= incomplete ->
					  {Decoded2,erlang:iolist_to_binary(Rest2)};
				      {error, <<>>, _} ->
					  exit(invalid_unicode);
				      {incomplete, <<>>, R} ->
					  {<<>>,R};
				      Binary when is_binary(Binary) ->
					  {Binary,<<>>}
				  end;
			      _ -> %i.e. eof
				  {Data0,<<>>}
			  end,
	case catch Mod:Func(S0, Data1, OutEnc, XtraArg) of
	    {stop,Result,Buf} ->
		{reply,Result,State#state{buf = (if
						     is_binary(Buf) ->
							 list_to_binary([unicode:characters_to_binary(Buf,OutEnc,InEnc),NewBuff]);
						     is_list(Buf) ->
							 list_to_binary([unicode:characters_to_binary(Buf,unicode,InEnc),NewBuff]);
						     true ->
							 NewBuff
						end)}};
	    {'EXIT',Reason} ->
		{stop,Reason,{error,err_func(Mod, Func, XtraArg)},State};
	    S1 ->
		get_chars_notempty(Mod, Func, XtraArg, S1, OutEnc, State#state{buf=NewBuff})
	end
    catch
	exit:ExReason ->
            {stop,ExReason,invalid_unicode_error(Mod, Func, XtraArg, S0),State};
	error:ErrReason ->
	   {stop,ErrReason,{error,err_func(Mod, Func, XtraArg)},State}
    end.
	    
%% A hack that tries to inform the caller about the position where the
%% error occured.
invalid_unicode_error(Mod, Func, XtraArg, S) ->
    try
        {erl_scan,tokens,_Args} = XtraArg,
        Location = erl_scan:continuation_location(S),
        {error,{Location, ?MODULE, invalid_unicode},Location}
    catch
        _:_ ->
            {error,err_func(Mod, Func, XtraArg)}
    end.

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F.



%% Process the I/O request setopts
%%
%% setopts
setopts(Opts0,State) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{list,binary}], 
	       expand_encoding(Opts0))),
    case check_valid_opts(Opts) of
	true ->
	    do_setopts(Opts,State);
	false ->
	    {error,{error,enotsup},State}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,_}|T]) ->
    check_valid_opts(T);
check_valid_opts([{encoding,_Enc}|T]) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.
do_setopts(Opts, State) ->
    case valid_enc(proplists:get_value(encoding, Opts, State#state.unic)) of
	{ok,NewUnic} ->
	    case proplists:get_value(binary, Opts) of
		true ->
		    {reply,ok,State#state{read_mode=binary, unic=NewUnic}};
		false ->
		    {reply,ok,State#state{read_mode=list, unic=NewUnic}};
		undefined ->
		    {reply,ok,State#state{unic=NewUnic}}
	    end;
	_ ->
	    {error,{error,badarg},State} 
    end.

getopts(#state{read_mode=RM, unic=Unic} = State) ->
    Bin = {binary, RM =:= binary},
    Uni = {encoding, Unic},
    {reply,[Bin,Uni],State}.

%% Concatenate two binaries and convert the result to list or binary
cat(B1, B2, binary, latin1, latin1) ->
    list_to_binary([B1,B2]);
cat(B1, B2, binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary([B1,B2],InEncoding,OutEncoding) of
	Good when is_binary(Good) ->
	    Good;
	_ ->
	    exit({no_translation,InEncoding,OutEncoding})
    end;
%% Dialyzer finds this is never used...                                                       
%% cat(B1, B2, list, InEncoding, OutEncoding) when InEncoding =/= latin1 ->
%%     % Catch i.e. unicode -> latin1 errors by using the outencoding although otherwise
%%     % irrelevant for lists...
%%     try
%% 	unicode:characters_to_list(unicode:characters_to_binary([B1,B2],InEncoding,OutEncoding),
%% 				   OutEncoding)
%%     catch
%% 	error:_ ->
%% 	    exit({no_translation,InEncoding,OutEncoding})
%%     end.
cat(B1, B2, list, latin1,_) ->
    binary_to_list(B1)++binary_to_list(B2).

%% Cast binary to list or binary
cast(eof, _, _, _) ->
    eof;
cast(B, binary, latin1, latin1) ->
    B;
cast(B, binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary(B,InEncoding,OutEncoding) of
	Good when is_binary(Good) ->
	    Good;
	_ ->
	    exit({no_translation,InEncoding,OutEncoding})
    end;
cast(B, list, latin1, _) ->
    binary_to_list(B);
cast(B, list, InEncoding, OutEncoding) ->
    try
	unicode:characters_to_list(unicode:characters_to_binary(B,InEncoding,OutEncoding),
				   OutEncoding)
    catch
	error:_ ->
	    exit({no_translation,InEncoding,OutEncoding})
    end.

%% Convert buffer to binary
cast_binary(Binary) when is_binary(Binary) ->
    Binary;
cast_binary([<<>>|List]) ->
    cast_binary(List);
cast_binary(List) when is_list(List) ->
    list_to_binary(List);
cast_binary(_EOF) ->
    <<>>.

%% Read size for different read modes
read_size(binary) ->
    ?READ_SIZE_BINARY;
read_size(list) ->
    ?READ_SIZE_LIST.

%% Utf utility
count_and_find(Bin,N,Encoding) ->
    cafu(Bin,N,0,0,none,case Encoding of 
			   unicode -> utf8;
			   Oth -> Oth
			end).

cafu(<<>>,0,Count,ByteCount,_SavePos,_) ->
    {Count,ByteCount};
cafu(<<>>,_N,Count,_ByteCount,SavePos,_) ->
    {Count,SavePos};
cafu(<<_/utf8,Rest/binary>>, 0, Count, ByteCount, _SavePos, utf8) ->
    cafu(Rest,-1,Count+1,0,ByteCount,utf8);
cafu(<<_/utf8,Rest/binary>>, N, Count, _ByteCount, SavePos, utf8) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos,utf8);
cafu(<<_/utf8,Rest/binary>> = Whole, N, Count, ByteCount, SavePos, utf8) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos,utf8);
cafu(<<_/utf16-big,Rest/binary>>, 0, Count, ByteCount, _SavePos, {utf16,big}) ->
    cafu(Rest,-1,Count+1,0,ByteCount,{utf16,big});
cafu(<<_/utf16-big,Rest/binary>>, N, Count, _ByteCount, SavePos, {utf16,big}) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos,{utf16,big});
cafu(<<_/utf16-big,Rest/binary>> = Whole, N, Count, ByteCount, SavePos, {utf16,big}) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos,{utf16,big});
cafu(<<_/utf16-little,Rest/binary>>, 0, Count, ByteCount, _SavePos, {utf16,little}) ->
    cafu(Rest,-1,Count+1,0,ByteCount,{utf16,little});
cafu(<<_/utf16-little,Rest/binary>>, N, Count, _ByteCount, SavePos, {utf16,little}) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos,{utf16,little});
cafu(<<_/utf16-little,Rest/binary>> = Whole, N, Count, ByteCount, SavePos, {utf16,little}) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos,{utf16,little});
cafu(<<_/utf32-big,Rest/binary>>, 0, Count, ByteCount, _SavePos, {utf32,big}) ->
    cafu(Rest,-1,Count+1,0,ByteCount,{utf32,big});
cafu(<<_/utf32-big,Rest/binary>>, N, Count, _ByteCount, SavePos, {utf32,big}) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos,{utf32,big});
cafu(<<_/utf32-big,Rest/binary>> = Whole, N, Count, ByteCount, SavePos, {utf32,big}) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos,{utf32,big});
cafu(<<_/utf32-little,Rest/binary>>, 0, Count, ByteCount, _SavePos, {utf32,little}) ->
    cafu(Rest,-1,Count+1,0,ByteCount,{utf32,little});
cafu(<<_/utf32-little,Rest/binary>>, N, Count, _ByteCount, SavePos, {utf32,little}) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos,{utf32,little});
cafu(<<_/utf32-little,Rest/binary>> = Whole, N, Count, ByteCount, SavePos, {utf32,little}) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos,{utf32,little});
cafu(_Other,0,Count,ByteCount,_,_) -> % Non Unicode character, 
                                     % but found our point, OK this time
    {Count,ByteCount};
cafu(Other,_N,Count,0,SavePos,Enc) -> % Not enough, but valid chomped unicode
                                       % at end.
    case cbv(Enc,Other) of
	false ->
	    exit(invalid_unicode);
	_ ->
	    {Count,SavePos}
    end;
cafu(Other,_N,Count,ByteCount,none,Enc) -> % Return what we'we got this far
					   % although not complete, 
					   % it's not (yet) in error
    case cbv(Enc,Other) of
	false ->
	    exit(invalid_unicode);
	_ ->
	    {Count,ByteCount}
    end;
cafu(Other,_N,Count,_ByteCount,SavePos,Enc) -> % As above but we have 
					       % found a position
    case cbv(Enc,Other) of
	false ->
	    exit(invalid_unicode);
	_ ->
	    {Count,SavePos}
    end.

%%
%% Bluntly stolen from stdlib/unicode.erl (cbv means can be valid?)
%%
cbv(utf8,<<1:1,1:1,0:1,_:5>>) -> 
    1;
cbv(utf8,<<1:1,1:1,1:1,0:1,_:4,R/binary>>) -> 
    case R of
	<<>> ->
	    2;
	<<1:1,0:1,_:6>> ->
	    1;
	_ ->
	    false
    end;
cbv(utf8,<<1:1,1:1,1:1,1:1,0:1,_:3,R/binary>>) ->
    case R of
	<<>> ->
	    3;
	<<1:1,0:1,_:6>> ->
	    2;
	<<1:1,0:1,_:6,1:1,0:1,_:6>> ->
	    1;
	_ ->
	    false
    end;
cbv(utf8,_) ->
    false;

cbv({utf16,big},<<A:8>>) when A =< 215; A >= 224 ->
    1;
cbv({utf16,big},<<54:6,_:2>>) ->
    3;
cbv({utf16,big},<<54:6,_:10>>) ->
    2;
cbv({utf16,big},<<54:6,_:10,55:6,_:2>>) ->
    1;
cbv({utf16,big},_) ->
    false;
cbv({utf16,little},<<_:8>>) ->
    1; % or 3, we'll see
cbv({utf16,little},<<_:8,54:6,_:2>>) ->
    2;
cbv({utf16,little},<<_:8,54:6,_:2,_:8>>) ->
    1;
cbv({utf16,little},_) ->
    false;


cbv({utf32,big}, <<0:8>>) ->
    3;
cbv({utf32,big}, <<0:8,X:8>>) when X =< 16 ->
    2;
cbv({utf32,big}, <<0:8,X:8,Y:8>>) 
  when X =< 16, ((X > 0) or ((Y =< 215) or (Y >= 224))) ->
    1;
cbv({utf32,big},_) ->
    false;
cbv({utf32,little},<<_:8>>) ->
    3;
cbv({utf32,little},<<_:8,_:8>>) -> 
    2;
cbv({utf32,little},<<X:8,255:8,0:8>>) when X =:= 254; X =:= 255 ->
    false;
cbv({utf32,little},<<_:8,Y:8,X:8>>) 
  when X =< 16, ((X > 0) or ((Y =< 215) or (Y >= 224))) ->
    1;
cbv({utf32,little},_) ->
    false.


%%%-----------------------------------------------------------------
%%% ?PRIM_FILE helpers

%% Compensates ?PRIM_FILE:position/2 for the number of bytes 
%% we have buffered
position(Handle, At, Buf) ->
    ?PRIM_FILE:position(
       Handle,
       case At of
	   cur ->
	       {cur, -byte_size(Buf)};
	   {cur, Offs} ->
	       {cur, Offs-byte_size(Buf)};
	   _ ->
	       At
       end).
