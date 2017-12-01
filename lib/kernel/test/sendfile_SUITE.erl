%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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

-module(sendfile_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2]).

-export([sendfile_server/2, sendfile_do_recv/2, init/1, handle_event/2]).

-export(
    [t_sendfile_small/1,
     t_sendfile_big_all/1,
     t_sendfile_big_size/1,
     t_sendfile_many_small/1,
     t_sendfile_partial/1,
     t_sendfile_offset/1,
     t_sendfile_sendafter/1,
     t_sendfile_recvafter/1,
     t_sendfile_recvafter_remoteclose/1,
     t_sendfile_sendduring/1,
     t_sendfile_recvduring/1,
     t_sendfile_closeduring/1,
     t_sendfile_crashduring/1,
     t_sendfile_arguments/1]).

all() ->
    [t_sendfile_small,
     t_sendfile_big_all,
     t_sendfile_big_size,
     t_sendfile_many_small,
     t_sendfile_partial,
     t_sendfile_offset,
     t_sendfile_sendafter,
     t_sendfile_recvafter,
     t_sendfile_recvafter_remoteclose,
     t_sendfile_sendduring,
     t_sendfile_recvduring,
     t_sendfile_closeduring,
     t_sendfile_crashduring,
     t_sendfile_arguments].

init_per_suite(Config) ->
    case {os:type(),os:version()} of
	{{unix,sunos}, {5,8,_}} ->
	    {skip, "Solaris 8 not supported for now"};
	_ ->
	    Priv = proplists:get_value(priv_dir, Config),
	    SFilename = filename:join(Priv, "sendfile_small.html"),
	    {ok, DS} = file:open(SFilename,[write,raw]),
	    file:write(DS,"yo baby yo"),
	    file:sync(DS),
	    file:close(DS),
	    BFilename = filename:join(Priv, "sendfile_big.html"),
	    {ok, DB} = file:open(BFilename,[write,raw]),
	    [file:write(DB,[<<0:(10*8*1024*1024)>>]) || _I <- lists:seq(1,51)],
	    file:sync(DB),
	    file:close(DB),
	    [{small_file, SFilename},
	     {file_opts,[raw,binary]},
	     {big_file, BFilename}|Config]
    end.

end_per_suite(Config) ->
    file:delete(proplists:get_value(big_file, Config)).

init_per_testcase(TC,Config) when TC == t_sendfile_recvduring;
				  TC == t_sendfile_sendduring ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {_Size, Data} = sendfile_file_info(Filename),
		   {ok,Fd} = file:open(Filename, [raw,binary,read]),
                   %% Determine whether the driver has native support by
                   %% hitting the raw module directly; file:sendfile/5 will
                   %% land in the fallback if it doesn't.
                   RawModule = Fd#file_descriptor.module,
                   {ok, _Ignored} = RawModule:sendfile(Fd,Sock,0,0,0,[],[],[]),
		   Data
	   end,

    %% Check if sendfile is supported on this platform
    case catch sendfile_send(Send) of
	ok ->
	    init_per_testcase(t_sendfile, Config);
	Error ->
	    ct:log("Error: ~p",[Error]),
	    {skip,"Not supported"}
    end;
init_per_testcase(_TC,Config) ->
    Config.

t_sendfile_small(Config) when is_list(Config) ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   %% Here we make sure to test the sendfile/2 api
		   {ok, Size} = file:sendfile(Filename, Sock),
		   Data
	   end,

    ok = sendfile_send(Send).

t_sendfile_many_small(Config) when is_list(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    error_logger:add_report_handler(?MODULE,[self()]),

    Send = fun(Sock) ->
		   {Size,_} = sendfile_file_info(Filename),
		   N = 10000,
		   {ok,D} = file:open(Filename,[read|FileOpts]),
		   [begin
			{ok,Size} = file:sendfile(D,Sock,0,0,SendfileOpts)
		    end || _I <- lists:seq(1,N)],
		   file:close(D),
		   Size*N
	   end,

    ok = sendfile_send({127,0,0,1}, Send, 0),

    receive
	{stolen,Reason} ->
	    exit(Reason)
    after 200 ->
	    ok
    end.


t_sendfile_big_all(Config) when is_list(Config) ->
    Filename = proplists:get_value(big_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   {ok, Size} = sendfile(Filename, Sock, SendfileOpts),
		   Size
	   end,

    ok = sendfile_send({127,0,0,1}, Send, 0).

t_sendfile_big_size(Config) ->
    Filename = proplists:get_value(big_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    SendAll = fun(Sock) ->
		      {ok, #file_info{size = Size}} =
			  file:read_file_info(Filename),
		      {ok,D} = file:open(Filename,[read|FileOpts]),
		      {ok, Size} = file:sendfile(D, Sock,0,Size,SendfileOpts),
		      Size
	      end,

    ok = sendfile_send({127,0,0,1}, SendAll, 0).

t_sendfile_partial(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    SendSingle = fun(Sock) ->
			 {_Size, <<Data:5/binary,_/binary>>} =
			     sendfile_file_info(Filename),
			 {ok,D} = file:open(Filename,[read|FileOpts]),
			 {ok,5} = file:sendfile(D,Sock,0,5,SendfileOpts),
			 file:close(D),
			 Data
		 end,
    ok = sendfile_send(SendSingle),

    {_Size, <<FData:5/binary,SData:3/binary,_/binary>>} =
	sendfile_file_info(Filename),
    {ok,D} = file:open(Filename,[read|FileOpts]),
    {ok, <<FData/binary>>} = file:read(D,5),
    FSend = fun(Sock) ->
		    {ok,5} = file:sendfile(D,Sock,0,5,SendfileOpts),
		    FData
	    end,

    ok = sendfile_send(FSend),

    SSend = fun(Sock) ->
		    {ok,3} = file:sendfile(D,Sock,5,3,SendfileOpts),
		    SData
	    end,

    ok = sendfile_send(SSend),

    {ok, <<SData/binary>>} = file:read(D,3),

    file:close(D).

t_sendfile_offset(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock) ->
		   {_Size, <<_:5/binary,Data:3/binary,_/binary>> = AllData} =
		       sendfile_file_info(Filename),
		   {ok,D} = file:open(Filename,[read|FileOpts]),
		   {ok,3} = file:sendfile(D,Sock,5,3,SendfileOpts),
		   {ok, AllData} = file:read(D,100),
		   file:close(D),
		   Data
	   end,
    ok = sendfile_send(Send).


t_sendfile_sendafter(Config) ->
    Filename = proplists:get_value(small_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok, Size} = sendfile(Filename, Sock, SendfileOpts),
		   ok = gen_tcp:send(Sock, <<2>>),
		   <<Data/binary,2>>
	   end,

    ok = sendfile_send(Send).

t_sendfile_recvafter(Config) ->
    Filename = proplists:get_value(small_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok, Size} = sendfile(Filename, Sock, SendfileOpts),
		   ok = gen_tcp:send(Sock, <<1>>),
		   {ok,<<1>>} = gen_tcp:recv(Sock, 1),
		   <<Data/binary,1>>
	   end,

    ok = sendfile_send(Send).

%% This tests specifically for a bug fixed in 17.0
t_sendfile_recvafter_remoteclose(Config) ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock, SFServer) ->
		   {Size, _Data} = sendfile_file_info(Filename),
		   {ok, Size} = file:sendfile(Filename, Sock),

		   %% Make sure the remote end has been closed
		   SFServer ! stop,
		   timer:sleep(100),

		   %% In the bug this returned {error,ebadf}
		   {error,closed} = gen_tcp:recv(Sock, 1),
		   -1
	   end,

    ok = sendfile_send({127,0,0,1},Send,0).

t_sendfile_sendduring(Config) ->
    Filename = proplists:get_value(big_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   spawn_link(fun() ->
				      timer:sleep(50),
				      ok = gen_tcp:send(Sock, <<2>>)
			      end),
		   {ok, Size} = sendfile(Filename, Sock, SendfileOpts),
		   Size+1
	   end,

    ok = sendfile_send({127,0,0,1}, Send, 0).

t_sendfile_recvduring(Config) ->
    Filename = proplists:get_value(big_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   spawn_link(fun() ->
				      timer:sleep(50),
				      ok = gen_tcp:send(Sock, <<1>>),
				      {ok,<<1>>} = gen_tcp:recv(Sock, 1)
			      end),
		   {ok, Size} = sendfile(Filename, Sock, SendfileOpts),
		   timer:sleep(1000),
		   Size+1
	   end,

    ok = sendfile_send({127,0,0,1}, Send, 0).

t_sendfile_closeduring(Config) ->
    Filename = proplists:get_value(big_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    Send = fun(Sock,SFServPid) ->
		   spawn_link(fun() ->
				      timer:sleep(50),
				      SFServPid ! stop
			      end),
		   case erlang:system_info(thread_pool_size) of
		       0 ->
			   {error, closed} = sendfile(Filename, Sock,
						      SendfileOpts);
		       _Else ->
			   %% This can return how much has been sent or
			   %% {error,closed} depending on OS.
			   %% How much is sent impossible to know as
			   %%  the socket was closed mid sendfile
			   case sendfile(Filename, Sock, SendfileOpts) of
			       {error, closed} ->
				   ok;
			       {ok, Size}  when is_integer(Size) ->
				   ok
			   end
		   end,
		   -1
	   end,

    ok = sendfile_send({127,0,0,1}, Send, 0).

t_sendfile_crashduring(Config) ->
    Filename = proplists:get_value(big_file, Config),
    SendfileOpts = proplists:get_value(sendfile_opts, Config, []),

    error_logger:add_report_handler(?MODULE,[self()]),

    Send = fun(Sock) ->
		   spawn_link(fun() ->
				      timer:sleep(50),
				      exit(die)
			      end),
		   {error, closed} = sendfile(Filename, Sock, SendfileOpts),
		   -1
	   end,
    process_flag(trap_exit,true),
    spawn_link(fun() ->
		       ok = sendfile_send({127,0,0,1}, Send, 0)
	       end),
    receive
	{stolen,Reason} ->
	    process_flag(trap_exit,false),
	    ct:fail(Reason)
	after 200 ->
		receive
		    {'EXIT',_,Reason} ->
			process_flag(trap_exit,false),
			die = Reason
		end
	end.

t_sendfile_arguments(Config) ->
    Filename = proplists:get_value(small_file, Config),

    {ok, Listener} = gen_tcp:listen(0,
        [{packet, 0}, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listener),

    ErrorCheck =
        fun(Reason, Offset, Length, Opts) ->
            {ok, Sender} = gen_tcp:connect({127, 0, 0, 1}, Port,
                [{packet, 0}, {active, false}]),
            {ok, Receiver} = gen_tcp:accept(Listener),
            {ok, Fd} = file:open(Filename, [read, raw]),
            {error, Reason} = file:sendfile(Fd, Sender, Offset, Length, Opts),
            gen_tcp:close(Receiver),
            gen_tcp:close(Sender),
            file:close(Fd)
        end,

    ErrorCheck(einval, -1, 0, []),
    ErrorCheck(einval, 0, -1, []),
    ErrorCheck(badarg, gurka, 0, []),
    ErrorCheck(badarg, 0, gurka, []),
    ErrorCheck(badarg, 0, 0, gurka),
    ErrorCheck(badarg, 0, 0, [{chunk_size, gurka}]),

    gen_tcp:close(Listener),

    ok.

%% Generic sendfile server code
sendfile_send(Send) ->
    sendfile_send({127,0,0,1},Send).
sendfile_send(Host, Send) ->
    sendfile_send(Host, Send, []).
sendfile_send(Host, Send, Orig) ->
    SFServer = spawn_link(?MODULE, sendfile_server, [self(), Orig]),
    receive
	{server, Port} ->
	    {ok, Sock} = gen_tcp:connect(Host, Port,
					       [binary,{packet,0},
						{active,false}]),
	    Data = case proplists:get_value(arity,erlang:fun_info(Send)) of
		       1 ->
			   Send(Sock);
		       2 ->
			   Send(Sock, SFServer)
		   end,
	    ok = gen_tcp:close(Sock),
	    receive
		{ok, Bin} ->
		    Data = Bin,
		    ok
	    end
    end.

sendfile_server(ClientPid, Orig) ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 0},
				     {active, true},
				     {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    ClientPid ! {server, Port},
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = sendfile_do_recv(Sock, Orig),
    ClientPid ! {ok, Bin},
    gen_tcp:send(Sock, <<1>>).

-define(SENDFILE_TIMEOUT, 10000).
sendfile_do_recv(Sock, Bs) ->
    TimeoutMul = case os:type() of
		     {win32, _} -> 6;
		     _ -> 1
		 end,
    receive
	stop when Bs /= 0,is_integer(Bs) ->
	    gen_tcp:close(Sock),
	    {ok, -1};
	{tcp, Sock, B} ->
	    case binary:match(B,<<1>>) of
		nomatch when is_list(Bs) ->
		    sendfile_do_recv(Sock, [B|Bs]);
		nomatch when is_integer(Bs) ->
		    sendfile_do_recv(Sock, byte_size(B) + Bs);
		_ when is_list(Bs) ->
		    ct:log("Stopped due to a 1"),
		    {ok, iolist_to_binary(lists:reverse([B|Bs]))};
		_ when is_integer(Bs) ->
		    ct:log("Stopped due to a 1"),
		    {ok, byte_size(B) + Bs}
	    end;
	{tcp_closed, Sock} when is_list(Bs) ->
	    ct:log("Stopped due to close"),
	    {ok, iolist_to_binary(lists:reverse(Bs))};
	{tcp_closed, Sock} when is_integer(Bs) ->
	    ct:log("Stopped due to close"),
	    {ok, Bs}
    after ?SENDFILE_TIMEOUT * TimeoutMul ->
	    ct:log("Sendfile timeout"),
	    timeout
    end.

sendfile_file_info(File) ->
    {ok, #file_info{size = Size}} = file:read_file_info(File),
    {ok, Data} = file:read_file(File),
    {Size, Data}.

sendfile(Filename,Sock,Opts) ->
    case file:open(Filename, [read, raw, binary]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Fd} ->
	    Res = file:sendfile(Fd, Sock, 0, 0, Opts),
	    _ = file:close(Fd),
	    Res
    end.


%% Error handler 

init([Proc]) -> {ok,Proc}.

handle_event({error,noproc,{emulator,Format,Args}}, Proc) -> 
    Proc ! {stolen,lists:flatten(io_lib:format(Format,Args))},
    {ok,Proc};
handle_event(_, Proc) -> 
    {ok,Proc}.
