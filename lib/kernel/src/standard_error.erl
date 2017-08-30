%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(standard_error).
-behaviour(supervisor_bridge).

%% Basic standard i/o server for user interface port.
-export([start_link/0, init/1, terminate/2]).

-define(NAME, standard_error).
-define(PROCNAME_SUP, standard_error_sup).

%% Defines for control ops
-define(CTRL_OP_GET_WINSIZE,100).

%%
%% The basic server and start-up.
%%
-spec start_link() -> 'ignore' | {'error',term()} | {'ok',pid()}.

start_link() ->
    supervisor_bridge:start_link({local, ?PROCNAME_SUP}, ?MODULE, []).

-spec terminate(term(), pid()) -> 'ok'.

terminate(_Reason,Pid) ->
    (catch exit(Pid,kill)),
    ok.

-spec init([]) -> {'error','no_stderror'} | {'ok',pid(),pid()}.

init([]) ->
    case (catch start_port([out,binary])) of
	Pid when is_pid(Pid) ->
	    {ok,Pid,Pid};
	_ ->
	    {error,no_stderror}
    end.

start_port(PortSettings) ->
    Id = spawn(fun () -> server({fd,2,2}, PortSettings) end),
    register(?NAME, Id),
    Id.

server(PortName,PortSettings) ->
    process_flag(trap_exit, true),
    Port = open_port(PortName,PortSettings),
    run(Port).

run(P) ->
    put(encoding, latin1),
    server_loop(P).

server_loop(Port) ->
    receive
	{io_request,From,ReplyAs,Request} when is_pid(From) ->
	    _ = do_io_request(Request, From, ReplyAs, Port),
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
	List when length(List) =:= 8 ->
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
%% Encoding option (unicode/latin1)
io_request({put_chars,unicode,Chars}, Port) ->
    case wrap_characters_to_binary(Chars, unicode, get(encoding)) of
        error ->
            {error,{error,put_chars}};
        Bin ->
            put_chars(Bin, Port)
    end;
io_request({put_chars,unicode,Mod,Func,Args}, Port) ->
    case catch apply(Mod, Func, Args) of
        Data when is_list(Data); is_binary(Data) ->
            case wrap_characters_to_binary(Data, unicode, get(encoding)) of
                Bin when is_binary(Bin) ->
                    put_chars(Bin, Port);
                error ->
                    {error,{error,put_chars}}
            end;
        _ ->
            {error,{error,put_chars}}
    end;
io_request({put_chars,latin1,Chars}, Port) ->
    case catch unicode:characters_to_binary(Chars, latin1, get(encoding)) of
        Data when is_binary(Data) ->
            put_chars(Data, Port);
        _ ->
            {error,{error,put_chars}}
    end;
io_request({put_chars,latin1,Mod,Func,Args}, Port) ->
    case catch apply(Mod, Func, Args) of
        Data when is_list(Data); is_binary(Data) ->
            case
                catch unicode:characters_to_binary(Data, latin1, get(encoding))
            of
                Bin when is_binary(Bin) ->
                    put_chars(Bin, Port);
                _ ->
                    {error,{error,put_chars}}
            end;
        _ ->
            {error,{error,put_chars}}
    end;
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
io_request(getopts, _Port) ->
    getopts();
io_request({setopts,Opts}, _Port) when is_list(Opts) ->
    setopts(Opts);
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
    _ = put_port(Chars, Port),
    {ok,ok}.

%% setopts
setopts(Opts0) ->
    Opts = expand_encoding(Opts0),
    case check_valid_opts(Opts) of
        true ->
            do_setopts(Opts);
        false ->
            {error,{error,enotsup}}
    end.

check_valid_opts([]) ->
    true;
check_valid_opts([{encoding,Valid}|T]) when Valid =:= unicode;
                                            Valid =:= utf8; Valid =:= latin1 ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

expand_encoding([]) ->
    [];
expand_encoding([latin1 | T]) ->
    [{encoding,latin1} | expand_encoding(T)];
expand_encoding([unicode | T]) ->
    [{encoding,unicode} | expand_encoding(T)];
expand_encoding([H|T]) ->
    [H|expand_encoding(T)].

do_setopts(Opts) ->
    case proplists:get_value(encoding, Opts) of
        Valid when Valid =:= unicode; Valid =:= utf8 ->
            put(encoding, unicode);
        latin1 ->
            put(encoding, latin1);
        undefined ->
            ok
    end,
    {ok,ok}.

getopts() ->
    Uni = {encoding,get(encoding)},
    {ok,[Uni]}.

wrap_characters_to_binary(Chars,From,To) ->
    TrNl = (whereis(user_drv) =/= undefined),
    Limit = case To of 
		latin1 ->
		    255;
		_Else ->
		    16#10ffff
	    end,
    case catch unicode:characters_to_list(Chars, From) of
        L when is_list(L) ->
            unicode:characters_to_binary(
              [ case X of
                    $\n when TrNl ->
                        "\r\n";
                    High when High > Limit ->
                        ["\\x{",erlang:integer_to_list(X, 16),$}];
                    Low ->
                        Low
                end || X <- L ], unicode, To);
        _ ->
            error
    end.
