%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
-module(standard_error).
-behaviour(supervisor_bridge).

%% Basic standard i/o server for user interface port.
-export([start_link/0, init/1, terminate/2]).

-define(NAME, standard_error).
-define(PROCNAME_SUP, standard_error_sup).
%% Internal exports
-export([server/1, server/2]).

%% Defines for control ops
-define(CTRL_OP_GET_WINSIZE,100).

%%
%% The basic server and start-up.
%%
start_link() ->
    supervisor_bridge:start_link({local, ?PROCNAME_SUP}, ?MODULE, []).

terminate(_Reason,Pid) ->
    (catch exit(Pid,kill)),
    ok.

init([]) ->
    case (catch start_port([out,binary])) of
	Pid when is_pid(Pid) ->
	    {ok,Pid,Pid};
	_ ->
	    {error,no_stderror}
    end.


start_port(PortSettings) ->
    Id = spawn(?MODULE,server,[{fd,2,2},PortSettings]),
    register(?NAME,Id),
    Id.


server(Pid) when is_pid(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    run(Pid).

server(PortName,PortSettings) ->
    process_flag(trap_exit, true),
    Port = open_port(PortName,PortSettings),
    run(Port).

run(P) ->
    put(unicode,false),
    server_loop(P).

server_loop(Port) ->
    receive
	{io_request,From,ReplyAs,Request} when is_pid(From) ->
	    do_io_request(Request, From, ReplyAs, Port),
	    server_loop(Port);
	{'EXIT',Port,badsig} ->			% Ignore badsig errors
	    server_loop(Port);
	{'EXIT',Port,What} ->			% Port has exited
	    exit(What);
	_Other ->				% Ignore other messages
	    server_loop(Port)
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

do_io_request(Req, From, ReplyAs, Port) ->
    {_Status,Reply}  = io_request(Req, Port),
    io_reply(From, ReplyAs, Reply).

%% New in R13B
% Wide characters (Unicode)
io_request({put_chars,Encoding,Chars}, Port) -> % Binary new in R9C
    put_chars(wrap_characters_to_binary(Chars,Encoding,
					case get(unicode) of 
					    true -> unicode;
					    _ -> latin1
					end), Port);
io_request({put_chars,Encoding,Mod,Func,Args}, Port) ->
    Result = case catch apply(Mod,Func,Args) of
		 Data when is_list(Data); is_binary(Data) ->
		     wrap_characters_to_binary(Data,Encoding,
					       case get(unicode) of 
						   true -> unicode;
						   _ -> latin1
					       end);
		 Undef ->
		     Undef
	     end,
    put_chars(Result, Port);
%% BC if called from pre-R13 node
io_request({put_chars,Chars}, Port) -> 
    io_request({put_chars,latin1,Chars}, Port); 
io_request({put_chars,Mod,Func,Args}, Port) ->
    io_request({put_chars,latin1,Mod,Func,Args}, Port);
%% New in R12
io_request({get_geometry,columns},Port) ->
    case get_fd_geometry(Port) of
	{W,_H} ->
	    {ok,W};
	_ ->
	    {error,{error,enotsup}}
    end;
io_request({get_geometry,rows},Port) ->
    case get_fd_geometry(Port) of
	{_W,H} ->
	    {ok,H};
	_ ->
	    {error,{error,enotsup}}
    end;
io_request({getopts,[]}, Port) ->
    getopts(Port);
io_request({setopts,Opts}, Port) when is_list(Opts) ->
    setopts(Opts, Port);
io_request({requests,Reqs}, Port) ->
    io_requests(Reqs, {ok,ok}, Port);
io_request(R, _Port) ->                      %Unknown request
    {error,{error,{request,R}}}.		%Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, Port)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,_Res}, Port) ->
    io_requests(Rs, io_request(R, Port), Port);
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
put_chars(Chars, Port) when is_binary(Chars) ->
    put_port(Chars, Port),
    {ok,ok};
put_chars(Chars, Port) ->
    case catch list_to_binary(Chars) of
	Binary when is_binary(Binary) ->
	    put_chars(Binary, Port);
	_ ->
	    {error,{error,put_chars}}
    end.

%% setopts
setopts(Opts0,Port) ->
    Opts = proplists:unfold(
	     proplists:substitute_negations(
	       [{latin1,unicode}], 
	       Opts0)),
    case check_valid_opts(Opts) of
	true ->
	    do_setopts(Opts,Port);
	false ->
	    {error,{error,enotsup}}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{unicode,Valid}|T]) when Valid =:= true; Valid =:= utf8; Valid =:= false ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

do_setopts(Opts, _Port) ->
    case proplists:get_value(unicode,Opts) of
	Valid when Valid =:= true; Valid =:= utf8 ->
	    put(unicode,true);
	false ->
	    put(unicode,false);
	undefined ->
	    ok
    end,
    {ok,ok}.

getopts(_Port) ->
    Uni = {unicode, case get(unicode) of
		       true ->
			   true;
		       _ ->
			   false
		   end},
    {ok,[Uni]}.

wrap_characters_to_binary(Chars,From,To) ->
    TrNl = (whereis(user_drv) =/= undefined),
    Limit = case To of 
		latin1 ->
		    255;
		_Else ->
		    16#10ffff
	    end,
    unicode:characters_to_binary(
      [ case X of
	    $\n ->
		if
		    TrNl ->
			"\r\n";
		    true ->
			$\n
		end;
	    High when High > Limit ->
		["\\x{",erlang:integer_to_list(X, 16),$}];
	    Ordinary ->
		Ordinary
	end || X <- unicode:characters_to_list(Chars,From) ],unicode,To).
